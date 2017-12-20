package gov.idaho.isp.saktrack.util;

public class PasswordSettings {
  private Integer minLen;
  private Integer maxLen;
  private Integer numberOfUpperCase;
  private Integer numberOfDigits;
  private Integer numberOfSpecialCharacters;
  private boolean considerDigitsSpecialCharacters;

  public Integer getMinLen() {
    return minLen;
  }

  public void setMinLen(Integer minLen) {
    this.minLen = minLen;
  }

  public Integer getMaxLen() {
    return maxLen;
  }

  public void setMaxLen(Integer maxLen) {
    this.maxLen = maxLen;
  }

  public Integer getNumberOfUpperCase() {
    return numberOfUpperCase;
  }

  public void setNumberOfUpperCase(Integer numberOfUpperCase) {
    this.numberOfUpperCase = numberOfUpperCase;
  }

  public Integer getNumberOfDigits() {
    return numberOfDigits;
  }

  public void setNumberOfDigits(Integer numberOfDigits) {
    this.numberOfDigits = numberOfDigits;
  }

  public Integer getNumberOfSpecialCharacters() {
    return numberOfSpecialCharacters;
  }

  public void setNumberOfSpecialCharacters(Integer numberOfSpecialCharacters) {
    this.numberOfSpecialCharacters = numberOfSpecialCharacters;
  }

  public boolean getConsiderDigitsSpecialCharacters() {
    return considerDigitsSpecialCharacters;
  }

  public void setConsiderDigitsSpecialCharacters(boolean considerDigitsSpecialCharacters) {
    this.considerDigitsSpecialCharacters = considerDigitsSpecialCharacters;
  }
}