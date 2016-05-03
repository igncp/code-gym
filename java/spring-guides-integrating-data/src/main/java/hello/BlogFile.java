package hello;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.PrintWriter;

public class BlogFile {
  private BufferedReader reader;
  private Integer waitPeriod = 1000;

  private File generateBlogFile() {
    return new File("/tmp/spring-example/infoq-articles.txt");
  }

  public BlogFile() throws Exception {
    reader = generateReader();
  }

  public void empty() {
    try {
      PrintWriter writer = new PrintWriter(generateBlogFile());
      writer.print("");
      writer.close();
    } catch (IOException x) {
      System.err.println(x);
    }
  }

  public void close() throws Exception {
    reader.close();
  }

  public void readOrWait() throws Exception {
    String line = reader.readLine();

    if (line == null)
      Thread.sleep(waitPeriod);
    else System.out.println(line);
  }

  private BufferedReader generateReader() throws Exception {
    File file = generateBlogFile();
    FileReader fr = new FileReader(file);
    return new BufferedReader(fr);
  }
}
