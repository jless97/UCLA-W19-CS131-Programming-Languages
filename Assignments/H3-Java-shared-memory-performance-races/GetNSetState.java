import java.util.concurrent.atomic.AtomicIntegerArray;

class GetNSetState implements State {
    private AtomicIntegerArray value;
    private byte maxval;
    private void initAtomicIntegerArray(byte[] v) {
      value = new AtomicIntegerArray(v.length);
      for (int i = 0; i < v.length; i++) value.set(i, v[i]);
    }

    GetNSetState(byte[] v) { 
      initAtomicIntegerArray(v); 
      maxval = 127; 
    }

    GetNSetState(byte[] v, byte m) { 
      initAtomicIntegerArray(v); 
      maxval = m; 
    }

    public int size() { return value.length(); }

    public byte[] current() { 
      byte[] v = new byte[value.length()];
      for (int i = 0; i < v.length; i++) v[i] = (byte) value.get(i);
      return v;
    }

    public boolean swap(int i, int j) {
      int vi = value.get(i), vj = value.get(j);
      if (vi <= 0 || vj >= maxval) return false;

      value.set(i, vi - 1);
      value.set(j, vj + 1);
      return true;
    }
}
