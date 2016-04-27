package hello;

public class Greeting {

    private final long id;
    private static final String template = "Hello, %s!";
    private final String content;

    public Greeting(long id, String user) {
        this.id = id;
        this.content = String.format(template, user);
    }

    public long getId() {
        return id;
    }

    public String getContent() {
        return content;
    }
}
