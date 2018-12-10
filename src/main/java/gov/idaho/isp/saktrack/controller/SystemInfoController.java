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
    return "admin/system-info";
  }
}