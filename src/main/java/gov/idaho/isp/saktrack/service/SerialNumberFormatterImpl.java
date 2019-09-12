/* 
 * Copyright 2017 Idaho State Police.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

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
