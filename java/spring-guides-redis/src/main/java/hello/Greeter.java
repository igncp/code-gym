package hello;

import org.springframework.context.ApplicationContext;
import org.springframework.data.redis.core.StringRedisTemplate;

public class Greeter {
  public void greet(ApplicationContext ctx) {
    StringRedisTemplate template = ctx.getBean(StringRedisTemplate.class);
    template.convertAndSend("foo-topic", "Heyya!");
  }
}
