package resemblance.job;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.apache.hadoop.conf.Configured;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.IntWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapred.FileInputFormat;
import org.apache.hadoop.mapred.FileOutputFormat;
import org.apache.hadoop.mapred.JobClient;
import org.apache.hadoop.mapred.JobConf;
import org.apache.hadoop.mapred.KeyValueTextInputFormat;
import org.apache.hadoop.mapred.MapReduceBase;
import org.apache.hadoop.mapred.Mapper;
import org.apache.hadoop.mapred.OutputCollector;
import org.apache.hadoop.mapred.Reducer;
import org.apache.hadoop.mapred.Reporter;
import org.apache.hadoop.mapred.SequenceFileOutputFormat;
import org.apache.hadoop.util.Tool;
import org.apache.hadoop.util.ToolRunner;

import resemblance.util.Sketcher;

/**
 * pass1 of the sketch dedupping
 * 
 * input is 
 *  doc_id \t space seperated text
 *  
 * output are the combos of sketch value pairs
 * needs to be further processed by combo_frequencies.pig
 */

public class SketchCombos extends Configured implements Tool {

  public static final int DFT_SKETCH_SIZE = 50;  // -D sketchSize
  public static final int DFT_SKETCH_SEED = 50;  // -D sketchSeed
  public static final int DFT_NGRAM_SIZE = 5;    // -D ngramSize
  
  public static void main(String args[]) throws Exception {
    ToolRunner.run(new SketchCombos(), args);
  }
    
  public int run(String[] args) throws Exception {
    
    if (args.length!=2) {
      throw new RuntimeException("usage: "+getClass().getName()+" <input> <output>");
    }
    
    JobConf conf = new JobConf(getConf(), getClass());
    conf.setJobName(getClass().getName());
    
    conf.setMapOutputKeyClass(IntWritable.class);
    conf.setMapOutputValueClass(Text.class);
    conf.setOutputKeyClass(Text.class);
    conf.setOutputValueClass(Text.class);
        
    conf.setInputFormat(KeyValueTextInputFormat.class);
    conf.setMapperClass(SketchCombosMapper.class);    
    conf.setReducerClass(SketchCombosReducer.class);    
    
    FileInputFormat.addInputPath(conf, new Path(args[0]));
    FileOutputFormat.setOutputPath(conf, new Path(args[1]+"/"+System.currentTimeMillis()));

    conf.set("mapred.output.compress", "true");
    conf.set("mapred.output.compression.type", "BLOCK");
//    conf.set("mapred.output.compression.codec", "org.apache.hadoop.io.compress.GzipCodec");
    conf.setOutputFormat(SequenceFileOutputFormat.class);
    
    JobClient.runJob(conf);

    return 0;
  }

  public static class SketchCombosMapper extends MapReduceBase implements Mapper<Text,Text,IntWritable,Text> {
    
    private int sketchSize, sketchSeed, ngramSize;
    
    public void configure(JobConf job) { 
      super.configure(job);
      sketchSize = job.getInt("sketchSize", DFT_SKETCH_SIZE);
      sketchSeed = job.getInt("sketchSeed", DFT_SKETCH_SEED);
      ngramSize  = job.getInt("ngramSize",  DFT_NGRAM_SIZE);
    }
        
    public void map(Text key, Text text, OutputCollector<IntWritable, Text> collector, Reporter reporter) throws IOException {      
      Sketcher sketcher = new Sketcher(sketchSize, sketchSeed);      
      int[] sketch = sketcher.sketchOf(text.toString(), ngramSize, Sketcher.TokeniseMode.TERM);
      for(int s : sketch) {
        collector.collect(new IntWritable(s), key);
      }
    }
    
  }

  public static class SketchCombosReducer extends MapReduceBase implements Reducer<IntWritable,Text,Text,Text> {
    public void reduce(IntWritable key, Iterator<Text> keys, OutputCollector<Text, Text> output, Reporter reporter) throws IOException {
      List<String> seenSoFar = new ArrayList<String>();
      
      // warning this algorithm is quadratic in the most frequent sketch value..
      while(keys.hasNext()) {
        String next = keys.next().toString();
        for(String seen : seenSoFar) {
          if (next.compareTo(seen) < 0) {
            output.collect(new Text(next), new Text(seen));
          }
          else {
            output.collect(new Text(seen), new Text(next));
          }
        }
        seenSoFar.add(next);
      }
      
      if (seenSoFar.size()==1) {
        // there was only one entry for this sketch value
        reporter.getCounter("exploded_combo","combo_size_unique").increment(1);
      } else {
        reporter.getCounter("exploded_combo","combo_size_log10_floor__"+Math.floor(Math.log10(seenSoFar.size()))).increment(1);
      }
      
    }
  }
  
  
  
}
