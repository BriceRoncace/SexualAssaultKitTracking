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

package gov.idaho.isp.saktrack.controller;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.SpringBootVersion;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;

@Controller
public class SystemInfoController {

  @GetMapping("/systemInfo")
  public String systemInfo(@Value("${project.version}") String version, Model m) {
    m.addAttribute("version", version);
    m.addAttribute("systemProperties", System.getProperties());
    m.addAttribute("springBootVersion", SpringBootVersion.getVersion());

    if (true) {
      throw new RuntimeException("Testing...");
    }

    return "admin/system-info";
  }
}