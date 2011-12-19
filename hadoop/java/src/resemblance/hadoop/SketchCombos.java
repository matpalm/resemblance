package resemblance.hadoop;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

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
import org.apache.hadoop.mapred.SequenceFileInputFormat;
import org.apache.hadoop.mapred.SequenceFileOutputFormat;
import org.apache.hadoop.util.Tool;
import org.apache.hadoop.util.ToolRunner;

import resemblance.util.Sketcher;


public class SketchCombos extends Configured implements Tool {

  public static final int SKETCH_SIZE = 10;
  public static final int SKETCH_SEED = 50;
  public static final int NGRAM_SHINGLE_LENGTH = 5; 
  
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
    
//    conf.setMaxMapTaskFailuresPercent(100);
//    conf.setNumReduceTasks(0);
    
    conf.setInputFormat(KeyValueTextInputFormat.class);
    conf.setMapperClass(SketchCombosMapper.class);    
    conf.setReducerClass(SketchCombosReducer.class);    
    
    FileInputFormat.addInputPath(conf, new Path(args[0]));
    FileOutputFormat.setOutputPath(conf, new Path(args[1]));

//    conf.set("mapred.output.compress", "true");
//    conf.set("mapred.output.compression.type", "BLOCK");
//    conf.set("mapred.output.compression.codec", "org.apache.hadoop.io.compress.GzipCodec");
//    conf.setOutputFormat(SequenceFileOutputFormat.class);
    
    JobClient.runJob(conf);

    return 0;
  }

  public static class SketchCombosMapper extends MapReduceBase implements Mapper<Text,Text,IntWritable,Text> {
    public void map(Text url, Text text, OutputCollector<IntWritable, Text> collector, Reporter reporter) throws IOException {      
      Sketcher sketcher = new Sketcher(SKETCH_SIZE, SKETCH_SEED);      
      int[] sketch = sketcher.sketchOf(text.toString(), NGRAM_SHINGLE_LENGTH, Sketcher.TokeniseMode.CHARACTER);
      IntWritable sketchValue = new IntWritable();
      for(int s : sketch) {
        sketchValue.set(s);
        collector.collect(sketchValue, url);
      }
    }    
  }

  public static class SketchCombosReducer extends MapReduceBase implements Reducer<IntWritable,Text,Text,Text> {
    public void reduce(IntWritable key, Iterator<Text> urls, OutputCollector<Text, Text> output, Reporter reporter) throws IOException {
      List<String> seenSoFar = new ArrayList<String>();
      while(urls.hasNext()) {
        String next = urls.next().toString();
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
