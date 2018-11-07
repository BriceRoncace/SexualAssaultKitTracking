package gov.idaho.isp.saktrack.mailer;

public enum MimeType {
  TEXT("text/plain; charset=utf-8", "\n"),
  HTML("text/html; charset=utf-8", "<br>");

  private final String label;
  private final String newLine;

  private MimeType(String label, String newLine) {
    this.label = label;
    this.newLine = newLine;
  }

  public String getLabel() {
    return label;
  }

  public boolean isHtml() {
    return HTML == this;
  }

  public String getNewLine() {
    return newLine;
  }
}
