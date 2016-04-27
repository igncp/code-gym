package hello;

import java.util.concurrent.atomic.AtomicLong;

import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
public class Router {
    private final AtomicLong counter = new AtomicLong();

    @RequestMapping("/greeting")
    public Greeting greeting(@RequestParam(value="user", defaultValue="User") String user) {
        return new Greeting(counter.incrementAndGet(), user);
    }
    
    @RequestMapping("/good-bye")
    public GoodBye goodBye(@RequestParam(value="place", defaultValue="Place") String place) {
        return new GoodBye(counter.incrementAndGet(), place);
    }
}
