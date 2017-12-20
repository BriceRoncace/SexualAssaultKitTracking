package gov.idaho.isp.saktrack;

public enum YesNoNa implements HasLabel {
  YES("Yes"),
  NO("No"),
  NA("N/A");

  private final String label;

  private YesNoNa(String label) {
    this.label = label;
  }

  @Override
  public String getLabel() {
    return label;
  }
}