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

import java.util.Random;

public class PasswordUtil {
  private static final String ALPHA_CAPS = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
  private static final String ALPHA = "abcdefghijklmnopqrstuvwxyz";
  private static final String NUM = "0123456789";
  private static final String SPL_CHARS = "!@#$%^&*_=+-/";

  public static String generatePasskey() {
    PasswordSettings settings = new PasswordSettings();
    settings.setMinLen(8);
    settings.setMaxLen(16);
    settings.setNumberOfUpperCase(2);
    settings.setNumberOfDigits(2);
    settings.setNumberOfSpecialCharacters(2);

    StringBuilder passkey = new StringBuilder();
    passkey.append(PasswordUtil.generatePassword(settings));
    return passkey.toString();
  }

  public static char[] generatePassword(PasswordSettings settings) {
    if (settings.getMinLen() > settings.getMaxLen()) {
      throw new IllegalArgumentException("Min. Length is greater than Max. Length!");
    }
    if ((settings.getNumberOfUpperCase() + settings.getNumberOfDigits()+ settings.getNumberOfSpecialCharacters()) > settings.getMinLen()) {
      throw new IllegalArgumentException("Min. Length should be at least sum of (CAPS, DIGITS, SPL CHARS) Length!");
    }
    Random rnd = new Random();
    int len = rnd.nextInt(settings.getMaxLen() - settings.getMinLen() + 1) + settings.getMinLen();
    char[] pswd = new char[len];
    int index = 0;
    for (int i = 0; i < settings.getNumberOfUpperCase(); i++) {
      index = getNextIndex(rnd, len, pswd);
      pswd[index] = ALPHA_CAPS.charAt(rnd.nextInt(ALPHA_CAPS.length()));
    }
    for (int i = 0; i < settings.getNumberOfDigits(); i++) {
      index = getNextIndex(rnd, len, pswd);
      pswd[index] = NUM.charAt(rnd.nextInt(NUM.length()));
    }
    for (int i = 0; i < settings.getNumberOfSpecialCharacters(); i++) {
      index = getNextIndex(rnd, len, pswd);
      pswd[index] = SPL_CHARS.charAt(rnd.nextInt(SPL_CHARS.length()));
    }
    for (int i = 0; i < len; i++) {
      if (pswd[i] == 0) {
        pswd[i] = ALPHA.charAt(rnd.nextInt(ALPHA.length()));
      }
    }
    return pswd;
  }

  public static boolean isValid(PasswordSettings settings, String password) {
    if (password.length() > settings.getMaxLen()) {
      return false;
    }
    if (password.length() < settings.getMinLen()) {
      return false;
    }
    if (PasswordUtil.getNumberOfUpperCase(password) < settings.getNumberOfUpperCase()) {
      return false;
    }

    if (settings.getConsiderDigitsSpecialCharacters()) {
      int digitsAndSpecials = PasswordUtil.getNumberOfDigits(password) + PasswordUtil.getNumberOfSpecialCharacters(password);
      if (digitsAndSpecials < settings.getNumberOfSpecialCharacters()) {
        return false;
      }
    }
    else {
      if (PasswordUtil.getNumberOfDigits(password) < settings.getNumberOfDigits()) {
        return false;
      }

      if (PasswordUtil.getNumberOfSpecialCharacters(password) < settings.getNumberOfSpecialCharacters()) {
        return false;
      }
    }

    return true;
  }

  public static int getNumberOfDigits(String str) {
    return str.replaceAll("\\D", "").length();
  }

  public static int getNumberOfUpperCase(String str) {
    return str.replaceAll("[^A-Z]", "").length();
  }

  public static int getNumberOfSpecialCharacters(String str) {
    return str.replaceAll("[^" + SPL_CHARS + "]", "").length();
  }

  private static int getNextIndex(Random rnd, int len, char[] pswd) {
    int index = rnd.nextInt(len);
    while (pswd[index = rnd.nextInt(len)] != 0);
    return index;
  }
}