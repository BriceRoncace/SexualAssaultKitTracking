package gov.idaho.isp.saktrack.organization;

import gov.idaho.isp.saktrack.jurisdiction.Jurisdiction;
import gov.idaho.isp.saktrack.validation.PasswordPolicy;
import java.io.Serializable;
import java.util.Objects;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Index;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.NamedQueries;
import javax.persistence.NamedQuery;
import javax.persistence.Table;
import javax.validation.constraints.NotNull;
import org.hibernate.validator.constraints.NotBlank;

@Entity
@Table(indexes = {@Index(name = "AK_Organization_name", columnList="name", unique = true)})
@NamedQueries({
  @NamedQuery(name = "Organization.findLegalByJusidictionId", query = "from Organization where type = 'LEGAL' and jurisdiction.id = ?1"),
  @NamedQuery(name = "Organization.findAssignableOrganizations", query = "from Organization where type != 'LEGAL' order by name")
})
public class Organization implements Serializable {
  public Organization () {}

  public Organization(Organization org) {
    this.id = org.getId();
    this.type = org.getType();
    this.name = org.getName();
    this.passkey = org.getPasskey();
    this.enabled = org.getEnabled();
    this.jurisdiction = org.getJurisdiction();
    this.allowLdapUsers = org.isAllowLdapUsers();
  }
  @Id
  @GeneratedValue(strategy=GenerationType.AUTO)
  private Long id;

  @NotNull(message = "{type.null}")
  @Enumerated(EnumType.STRING)
  private OrganizationType type;

  @NotBlank(message = "{name.blank}")
  private String name;

  @PasswordPolicy(minSize = 8, capitals = 1, numbers = 1, specials = 1)
  @NotBlank(message = "{passkey.blank}")
  private String passkey;

  private boolean enabled;

  @NotNull(message = "{jurisdiction.null}")
  @ManyToOne
  @JoinColumn(name = "jurisdictionId")
  private Jurisdiction jurisdiction;

  private boolean allowLdapUsers;

  public Long getId() {
    return id;
  }

  public void setId(Long id) {
    this.id = id;
  }

  public OrganizationType getType() {
    return type;
  }

  public void setType(OrganizationType type) {
    this.type = type;
  }

  public String getName() {
    return name;
  }

  public void setName(String name) {
    this.name = name;
  }

  public String getPasskey() {
    return passkey;
  }

  public void setPasskey(String passkey) {
    this.passkey = passkey;
  }

  public boolean getEnabled() {
    return enabled;
  }

  public void setEnabled(boolean enabled) {
    this.enabled = enabled;
  }

  public Jurisdiction getJurisdiction() {
    return jurisdiction;
  }

  public void setJurisdiction(Jurisdiction jurisdiction) {
    this.jurisdiction = jurisdiction;
  }

  public boolean isStatewide() {
    return jurisdiction != null ? jurisdiction.isStatewide() : false;
  }

  public boolean isAllowLdapUsers() {
    return allowLdapUsers;
  }

  public void setAllowLdapUsers(boolean allowLdapUsers) {
    this.allowLdapUsers = allowLdapUsers;
  }

  @Override
  public int hashCode() {
    int hash = 7;
    hash = 29 * hash + Objects.hashCode(this.type);
    hash = 29 * hash + Objects.hashCode(this.name);
    return hash;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) {
      return true;
    }
    if (obj == null) {
      return false;
    }
    if (getClass() != obj.getClass()) {
      return false;
    }
    final Organization other = (Organization) obj;
    if (!Objects.equals(this.name, other.name)) {
      return false;
    }
    if (this.type != other.type) {
      return false;
    }
    return true;
  }

  @Override
  public String toString() {
    return name;
  }
}