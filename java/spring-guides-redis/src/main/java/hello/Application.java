package hello;

import java.util.concurrent.CountDownLatch;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.ApplicationContext;

@SpringBootApplication
public class Application {
  public static void main(String[] args) throws InterruptedException {
    ApplicationContext ctx = SpringApplication.run(Application.class, args);
    CountDownLatch latch = ctx.getBean(CountDownLatch.class);

    new Greeter().greet(ctx);

    latch.await();
    System.exit(0);
  }
}