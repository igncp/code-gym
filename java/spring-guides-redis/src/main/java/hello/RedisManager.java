package hello;

import java.util.concurrent.CountDownLatch;

import org.springframework.context.annotation.Bean;
import org.springframework.data.redis.connection.RedisConnectionFactory;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.data.redis.listener.PatternTopic;
import org.springframework.data.redis.listener.RedisMessageListenerContainer;
import org.springframework.data.redis.listener.adapter.MessageListenerAdapter;
import org.springframework.stereotype.Service;

@Service
public class RedisManager {
  @Bean
  RedisMessageListenerContainer container(
      RedisConnectionFactory connectionFactory,
      MessageListenerAdapter listenerAdapter) {
    RedisMessageListenerContainer container = new RedisMessageListenerContainer();
    container.setConnectionFactory(connectionFactory);
    container.addMessageListener(listenerAdapter,
        new PatternTopic("foo-topic"));
    return container;
  }

  @Bean
  MessageListenerAdapter listenerAdapter(RedisReceiver receiver) {
    return new MessageListenerAdapter(receiver, "receiveMessage");
  }

  @Bean
  RedisReceiver receiver(CountDownLatch latch) {
    return new RedisReceiver(latch);
  }

  @Bean
  CountDownLatch latch() {
    return new CountDownLatch(1);
  }

  @Bean
  StringRedisTemplate template(RedisConnectionFactory connectionFactory) {
    return new StringRedisTemplate(connectionFactory);
  }
}
