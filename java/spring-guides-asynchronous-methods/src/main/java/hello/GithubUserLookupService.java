package hello;

import java.util.concurrent.Future;

import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.annotation.AsyncResult;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;

@Service
public class GithubUserLookupService {

  RestTemplate restTemplate = new RestTemplate();

  @Async
  public Future<User> findUser(String username) throws InterruptedException {
    CowLogger cl = new CowLogger();
    cl.log("Looking up " + username);

    User result = restTemplate
        .getForObject("https://api.github.com/users/" + username, User.class);

    Thread.sleep(1000L);
    return new AsyncResult<User>(result);
  }

}