package hello;

public class CowLogger {
  public void log(String unquotedStr) {
    try {
      new Command("cowsay '" + unquotedStr + "'");
    } catch (java.io.IOException e) {
    }
  }
}
