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
