package gov.idaho.isp.saktrack.controller.admin;

import gov.idaho.isp.ldap.user.LdapUser;
import gov.idaho.isp.ldap.user.LdapUserDirectory;
import gov.idaho.isp.ldap.user.LdapUserSearchCriteria;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

@RestController
public class LdapUserRestController {
  private final LdapUserDirectory ldapUserDirectory;

  public LdapUserRestController(LdapUserDirectory ldapUserDirectory) {
    this.ldapUserDirectory = ldapUserDirectory;
  }

  @RequestMapping(value="/ldapUser/lastNameLike/{lastName}", method = RequestMethod.GET)
  public List<LdapUserAdapter> findLdapUsersByLastNameLike(@PathVariable String lastName) {
    LdapUserSearchCriteria c = new LdapUserSearchCriteria();
    c.setLastNameLike(lastName);
    List<LdapUser> users = ldapUserDirectory.search(c);
    return users == null ? Collections.emptyList() : users.stream().map(LdapUserAdapter::new).collect(Collectors.toList());
  }

  public static class LdapUserAdapter {
    private String username;
    private String fullName;

    public LdapUserAdapter(LdapUser user) {
      this.username = user.getUsername();
      this.fullName = user.getFullName();
    }

    public String getUsername() {
      return username;
    }

    public void setUsername(String username) {
      this.username = username;
    }

    public String getFullName() {
      return fullName;
    }

    public void setFullName(String fullName) {
      this.fullName = fullName;
    }
  }
}
