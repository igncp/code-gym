package hello;

public class GoodBye {

    private final long id;
    private final String content;
    private static final String template = "Good bye: %s";

    public GoodBye(long id, String place) {
        this.id = id;
        this.content = String.format(template, place);
    }

    public long getId() {
        return id;
    }

    public String getContent() {
        return content;
    }
}
