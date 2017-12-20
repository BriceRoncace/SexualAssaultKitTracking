package gov.idaho.isp.saktrack.service;

import gov.idaho.isp.saktrack.exception.SexualAssaultKitTrackingException;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

@Service
public class SerialNumberFormatterImpl implements SerialNumberFormatter {
  private final String format;
  private final int length;

  public SerialNumberFormatterImpl(@Value("${serial.number.length}") int length) {
    this.length = length;
    this.format = "%0" + length + "d";
  }

  @Override
  public boolean isValid(String serialNumber) {
    return StringUtils.isNumeric(serialNumber);
  }

  @Override
  public String format(String serialNumber) {
    if (!isValid(serialNumber)) {
      throw new SexualAssaultKitTrackingException("Could not format non-numeric serial number [" + serialNumber + "]");
    }

    return serialNumber.length() < length ? format(Integer.valueOf(serialNumber)) : serialNumber;
  }

  @Override
  public String format(int serialNumber) {
    return String.format(format, serialNumber);
  }
}
