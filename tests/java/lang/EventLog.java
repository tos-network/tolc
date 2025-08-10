package java.lang;

public class EventLog {
    private String message;
    public EventLog(String message) {
        this.message = message;
    }

    public String getMessage() {
        return message;
    }
}