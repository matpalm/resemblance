package resemblance.util;

import java.util.HashSet;
import java.util.Random;
import java.util.Set;

public class Sketcher {
  
  public enum TokeniseMode { CHARACTER, TERM };
  
  private final int sketchSize;
  private final int[] seeds;
    
  public Sketcher(int sketchSize, long seed) {
    this.sketchSize = sketchSize;
    this.seeds = new int[sketchSize]; 
    
    Random r = new Random(seed);
    for (int i=0; i<seeds.length; i++) 
      seeds[i] = r.nextInt();
  }
  
  public int[] sketchOf(String text, int shingleLength, TokeniseMode mode) {
    Set<String> shingles =
      mode==TokeniseMode.CHARACTER ? 
          characterShinglesOf(text, shingleLength) :
          termShinglesOf(text, shingleLength);        
          
    int[] sketch = new int[sketchSize];
    for(int i=0; i<sketchSize; i++) {
      int min = Integer.MAX_VALUE;
      for(String shingle : shingles) {
        int hash = MurmurHash.hash(shingle, seeds[i]);
        if (hash < min)
          min = hash;
      }
      sketch[i] = min;
    }    
    
    return sketch;
  }
    
  private Set<String> characterShinglesOf(String str, int shingleLength) {
    Set<String> shingles = new HashSet<String>();    
    if (str.length() < shingleLength)
      return shingles;
    
    for(int i=0; i<str.length()-shingleLength+1; i++)
      shingles.add(str.substring(i, i+shingleLength));
    
    return shingles;
  }    

  private Set<String> termShinglesOf(String str, int shingleLength) {
    Set<String> shingles = new HashSet<String>();
    
    String[] tokens = str.split(" ");    
    if (tokens.length < shingleLength)
      return shingles;
    
    for(int i=0; i<tokens.length-shingleLength+1; i++) {
      StringBuilder sb = new StringBuilder();
      for(int j=i; j<i+shingleLength; j++) {
        sb.append(tokens[j]);
        if (j!=i+shingleLength-1) 
          sb.append(" ");
      }      
      shingles.add(sb.toString());
    }
    
    return shingles;
  }  
  
}
