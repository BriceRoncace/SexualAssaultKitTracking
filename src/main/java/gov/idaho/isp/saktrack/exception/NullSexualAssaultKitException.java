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

package gov.idaho.isp.saktrack.exception;

public class NullSexualAssaultKitException extends SexualAssaultKitTrackingException {

  public NullSexualAssaultKitException(String message, String... errors) {
    super(message, errors);
  }

  public NullSexualAssaultKitException(String message, Throwable cause, String... errors) {
    super(message, cause, errors);
  }
}
