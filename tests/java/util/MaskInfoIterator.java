package java.util;

public class MaskInfoIterator implements Iterator<MaskInfo> {
    private int basePartition;
    private int numPartitionsToTraverse;
    private int currentPartitionOffset;
    private int toIndex;
    private int currentFirstIndex;

    final static int  BITS_PER_LONG       = 64;
    final static int  BITS_PER_LONG_SHIFT = 6;
    final static long MASK                = 0xFFFFFFFFFFFFFFFFL;
  
    private static int longPosition(int index) {
        return index >> BITS_PER_LONG_SHIFT;
    }

    private static long getTrueMask(int fromIndex, int toIndex) {
        int currentRange = toIndex - fromIndex;
        return (MASK >>> (BITS_PER_LONG - currentRange)) << (fromIndex % BITS_PER_LONG);
    }

    public MaskInfoIterator(int fromIndex, int toIndex) {
      this.basePartition = longPosition(fromIndex);
      this.numPartitionsToTraverse = longPosition(toIndex - 1) - basePartition + 1;
      this.currentPartitionOffset = 0;
      this.toIndex = toIndex;
      this.currentFirstIndex = fromIndex;
    }

    public MaskInfo next() {
      int currentToIndex = Math.min(toIndex, (basePartition + currentPartitionOffset + 1) * BITS_PER_LONG);
      long mask = getTrueMask(currentFirstIndex, currentToIndex);
      MaskInfo info = new MaskInfo(mask, basePartition + currentPartitionOffset);
      currentFirstIndex = currentToIndex;
      currentPartitionOffset++;
      return info;
    }

    public boolean hasNext() {
      return currentPartitionOffset < numPartitionsToTraverse;
    }

    public void remove() {
      throw new UnsupportedOperationException();
    }

    public int getLastPartition() {
      return basePartition + numPartitionsToTraverse - 1;
    }
  }
