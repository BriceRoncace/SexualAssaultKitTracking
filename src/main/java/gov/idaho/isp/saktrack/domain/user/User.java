package gov.idaho.isp.saktrack.domain.user;

import org.springframework.security.core.userdetails.UserDetails;

public interface User extends UserDetails {
  public enum Type {
    ADMIN("admin", "admin"),
    LAB("lab", "lab"),
    MEDICAL("medical", "medical"),
    LAW_ENFORCEMENT("law-enforcement", "law-enforcement"),
    LEGAL("legal", "legal");

    private final String label;
    private final String devLabel;

    private Type(String label, String devLabel) {
      this.label = label;
      this.devLabel = devLabel;
    }

    public String getLabel() {
      return label;
    }

    public String getDevLabel() {
      return devLabel;
    }
  }

  Long getId();
  String getDisplayName();
  String getEmail();
  String getPhone();
  Type getType();
  boolean isAdmin();
  boolean isOrganizationAdmin();
  void setUserDetails(UserDetails userDetails);
}