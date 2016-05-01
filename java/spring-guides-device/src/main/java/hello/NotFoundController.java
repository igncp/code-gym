package hello;

import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseBody;

@Controller
public class NotFoundController {
  @RequestMapping("/**")
  public @ResponseBody String notFound() {
    String message = "Unknown page :(";
    return message;
  }
}
