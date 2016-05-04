package hello;

import java.util.concurrent.Future;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.CommandLineRunner;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.scheduling.annotation.EnableAsync;

@SpringBootApplication
@EnableAsync
public class Application implements CommandLineRunner {

  @Autowired
  GithubUserLookupService gitHubLookupService;

  @Override
  public void run(String... args) throws Exception {
    long start = System.currentTimeMillis();
    CowLogger cl = new CowLogger();

    String username = (args.length > 0) ? args[0] : "igncp";

    Future<User> page1 = gitHubLookupService.findUser(username);

    cl.log("User requested, doing other stuff...");

    while (!(page1.isDone())) {
      Thread.sleep(10);
    }

    cl.log("Elapsed time: " + (System.currentTimeMillis() - start));
    cl.log(page1.get().toString());
  }

  public static void main(String[] args) {
    SpringApplication.run(Application.class, args);
  }

}