package resemblance.job;

import java.io.DataInput;
import java.io.DataOutput;
import java.io.IOException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Iterator;

import org.apache.hadoop.conf.Configured;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.BytesWritable;
import org.apache.hadoop.io.IntWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.io.Writable;
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

public class RemoveExactDups extends Configured implements Tool {

  public static void main(String args[]) throws Exception {
    ToolRunner.run(new RemoveExactDups(), args);
  }
  
 public int run(String[] args) throws Exception {
    
    if (args.length!=2) {
      throw new RuntimeException("usage: "+getClass().getName()+" <input> <output>");
    }
    
    JobConf conf = new JobConf(getConf(), getClass());
    conf.setJobName(getClass().getName());
    
    conf.setMapOutputKeyClass(Text.class);
    conf.setMapOutputValueClass(Text.class);
    conf.setOutputKeyClass(Text.class);
    conf.setOutputValueClass(Text.class);
        
    conf.setInputFormat(KeyValueTextInputFormat.class);
    conf.setMapperClass(RemoveExactDupsMapper.class);    
    conf.setReducerClass(RemoveExactDupsReducer.class);    
    
    FileInputFormat.addInputPath(conf, new Path(args[0]));
    FileOutputFormat.setOutputPath(conf, new Path(args[1]));

    conf.set("mapred.output.compress", "true");
    conf.set("mapred.output.compression.type", "BLOCK");
//    conf.set("mapred.output.compression.codec", "org.apache.hadoop.io.compress.GzipCodec");
    conf.setOutputFormat(SequenceFileOutputFormat.class);
    
    JobClient.runJob(conf);

    return 0;
  }  
 
   public static class RemoveExactDupsMapper extends MapReduceBase implements Mapper<Text,Text,Text,Text> {
     
     public void map(Text key, Text text, OutputCollector<Text, Text> collector, Reporter reporter) throws IOException {
       
      try {
        MessageDigest md = MessageDigest.getInstance("MD5");
        md.reset();
        md.update(text.getBytes(), 0, text.getLength());
        byte[] digest = md.digest();
//        
//        StringBuffer sb = new StringBuffer();
//        for (int i = 0; i < digest.length; ++i) {
//          sb.append(Integer.toHexString((digest[i] & 0xFF) | 0x100).substring(1,3));
//        }
//        System.out.println("foo! "+sb.toString()+" ["+text.toString()+"]");
        
        // assume no tabs in key or text
        collector.collect(new Text(digest), new Text(key.toString()+"\t"+text.toString()));
        
      } catch (NoSuchAlgorithmException e) {
        throw new IOException("no md5 digest?");
      }
       
     }
     
   }
   
   public static class RemoveExactDupsReducer extends MapReduceBase implements Reducer<Text,Text,Text,Text> {
     
     public void reduce(Text textMD5, Iterator<Text> keyAndTexts, OutputCollector<Text, Text> output, Reporter reporter) throws IOException {
       // just assume first is canonical example, actually don't care about the key...
       Text canonicalExample = keyAndTexts.next();
       String[] keyAndText = canonicalExample.toString().split("\t");
       String key = keyAndText[0];
       String text = keyAndText[1];
       output.collect(new Text(key), new Text(text));
       
       // report number of dups
       int others = 0;
       while (keyAndTexts.hasNext()) {
         others++;
         keyAndTexts.next();
       }
       if (others>0) {         
         reporter.getCounter("remove_exact_dups", "dups").increment(others);
       }
     }
     
   }
   
//   public static class StringPair implements Writable {
//
//     private String key, text;
//     
//     public StringPair(String key, String text) {
//       this.key = key;
//       this.text = text;
//     }
//     
//     public void write(DataOutput out) throws IOException {
//       out.writeUTF(key);
//       out.writeUTF(text);
//     }
//     
//     public void readFields(DataInput in) throws IOException {
//       key = in.readUTF();
//       text = in.readUTF();
//     }
//     
//     public static StringPair read(DataInput in) throws IOException {
//       StringPair w = new StringPair(null, null);
//       w.readFields(in);
//       return w;
//     }
//     
//   }   
   
}
