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