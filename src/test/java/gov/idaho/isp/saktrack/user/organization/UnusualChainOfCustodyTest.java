package gov.idaho.isp.saktrack.user.organization;

import gov.idaho.isp.saktrack.ChainOfCustodyEvent;
import gov.idaho.isp.saktrack.SexualAssaultKit;
import gov.idaho.isp.saktrack.config.MockObjUtils;
import gov.idaho.isp.saktrack.jurisdiction.Jurisdiction;
import gov.idaho.isp.saktrack.organization.Organization;
import gov.idaho.isp.saktrack.organization.OrganizationType;
import org.junit.Assert;
import org.junit.Test;

public class UnusualChainOfCustodyTest {

  @Test
  public void abnormalTransitionTest() {
    Jurisdiction jurisdiction = MockObjUtils.createJurisdiction();
    Organization labOrg = MockObjUtils.createOrganization(OrganizationType.LAB, jurisdiction);
    LabUser labUser = MockObjUtils.createOrganizationUser(labOrg);
    SexualAssaultKit saKit = MockObjUtils.createSexualAssaultKit(labOrg);
    ChainOfCustodyEvent event1 = MockObjUtils.createChainOfCustodyEvent(ChainOfCustodyEvent.EventType.CREATE, labUser, null, labOrg);
    saKit.addChainOfCustodyEvent(event1);

    Assert.assertEquals(false, saKit.hasCalculatedMissingEvents());

    Organization lawOrg = MockObjUtils.createOrganization(OrganizationType.LAW_ENFORCEMENT, jurisdiction);
    Organization medOrg = MockObjUtils.createOrganization(OrganizationType.MEDICAL, jurisdiction);
    LawEnforcementUser lawUser = MockObjUtils.createOrganizationUser(lawOrg);
    ChainOfCustodyEvent event2 = MockObjUtils.createChainOfCustodyEvent(ChainOfCustodyEvent.EventType.RECEIVE, lawUser, medOrg, lawOrg);
    saKit.addChainOfCustodyEvent(event2);

    Assert.assertEquals(true, saKit.hasCalculatedMissingEvents());
  }
}
