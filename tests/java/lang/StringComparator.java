package java.lang;

import java.util.Comparator;

final class StringComparator implements Comparator<String> {
   StringComparator() {
   }

   @Override
   public int compare(String var1, String var2) {
      return var1.compareToIgnoreCase(var2);
   }
}

