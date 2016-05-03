package hello;

import org.springframework.boot.SpringApplication;
import org.springframework.context.ConfigurableApplicationContext;

public class BlogRunner {
  public void run() throws Exception {
    new BlogFile().empty();
    ConfigurableApplicationContext ctx = new SpringApplication(
        "/blog/integration.xml").run();
    readLoop();
    ctx.close();
  }

  private void readLoop() throws Exception {
    BlogFile blogFile = null;
    try {
      blogFile = new BlogFile();
      System.out.println(
          "Displaying a InfoQ RSS feed: " + "Press control+c to terminate");
      while (true)
        blogFile.readOrWait();
    } catch (InterruptedException ex) {
      blogFile.close();
    }
  }
}
