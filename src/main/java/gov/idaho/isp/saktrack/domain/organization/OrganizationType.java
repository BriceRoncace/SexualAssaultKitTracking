package gov.idaho.isp.saktrack.domain.organization;

import gov.idaho.isp.saktrack.domain.user.organization.AbstractOrganizationUser;
import gov.idaho.isp.saktrack.domain.user.organization.LabUser;
import gov.idaho.isp.saktrack.domain.user.organization.LawEnforcementUser;
import gov.idaho.isp.saktrack.domain.user.organization.LegalUser;
import gov.idaho.isp.saktrack.domain.user.organization.MedicalUser;

public enum OrganizationType {
  LAB("Lab") {
    @Override
    public AbstractOrganizationUser getNewUser() {
      return new LabUser();
    }
  },
  LAW_ENFORCEMENT("Law Enforcement") {
    @Override
    public AbstractOrganizationUser getNewUser() {
      return new LawEnforcementUser();
    }
  },
  MEDICAL("Medical") {
    @Override
    public AbstractOrganizationUser getNewUser() {
      return new MedicalUser();
    }
  },
  LEGAL("Legal") {
    @Override
    public AbstractOrganizationUser getNewUser() {
      return new LegalUser();
    }
  };

  private final String label;
  private final String devLabel;

  private OrganizationType(String label) {
    this.label = label;
    this.devLabel = getNewUser().getType().getDevLabel();
  }

  public abstract AbstractOrganizationUser getNewUser();

  public String getLabel() {
    return label;
  }

  public String getDevLabel() {
    return devLabel;
  }
}
