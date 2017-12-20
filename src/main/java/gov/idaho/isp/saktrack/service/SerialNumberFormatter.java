package gov.idaho.isp.saktrack.service;

public interface SerialNumberFormatter {
  boolean isValid(String serialNumber);
  String format(String serialNumber);
  String format(int serialNumber);
}
