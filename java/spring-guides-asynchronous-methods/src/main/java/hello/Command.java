package hello;

public class Command {
  public Command(String cmd) throws java.io.IOException {
    Process proc = Runtime.getRuntime().exec(cmd);
    java.io.InputStream is = proc.getInputStream();
    java.util.Scanner s = new java.util.Scanner(is).useDelimiter("\\A");
    String val = "";
    if (s.hasNext()) {
      val = s.next();
    } else {
      val = "";
    }
    s.close();
    System.out.println(val);
  }
}
