package java.util;

public class MaskInfo {
    public long mask;
    public int partitionIndex;

    public MaskInfo(long mask, int partitionIndex) {
      this.mask = mask;
      this.partitionIndex = partitionIndex;
    }
  }