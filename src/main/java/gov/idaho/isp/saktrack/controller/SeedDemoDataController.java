package gov.idaho.isp.saktrack.controller;

import javax.sql.DataSource;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Profile;
import org.springframework.core.io.ClassPathResource;
import org.springframework.core.io.Resource;
import org.springframework.jdbc.datasource.init.DatabasePopulator;
import org.springframework.jdbc.datasource.init.DatabasePopulatorUtils;
import org.springframework.jdbc.datasource.init.ResourceDatabasePopulator;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

@Controller
@Profile("dev")
public class SeedDemoDataController {
  private final String dbDriver;
  private final DataSource dataSource;

  public SeedDemoDataController(@Value("${spring.datasource.driver-class-name}") String dbDriver, DataSource dataSource) {
    this.dbDriver = dbDriver;
    this.dataSource = dataSource;
  }
  
  @PostMapping("/seedDemoData")
  public String seedDemoData(RedirectAttributes ra) {
    if ("org.h2.Driver".equals(dbDriver)) {
      try {
        Resource demoData = new ClassPathResource("sql/data-h2.sql");
        DatabasePopulator databasePopulator = new ResourceDatabasePopulator(demoData);
        DatabasePopulatorUtils.execute(databasePopulator, dataSource);
        ra.addFlashAttribute("messages", "Demo data imported into the H2 database.");
      }
      catch (RuntimeException ex) {
        ra.addFlashAttribute("errors", "Demo data failed to import (or has already been imported).");
      }
    }
    else {
      ra.addFlashAttribute("errors", "Demo data only intended for an H2 database.");
    }
    return "redirect:/";
  }
}