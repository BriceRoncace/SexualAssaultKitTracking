package gov.idaho.isp.saktrack.service;

import gov.idaho.isp.saktrack.config.MockObjUtils;
import gov.idaho.isp.saktrack.domain.ChainOfCustodyEvent;
import gov.idaho.isp.saktrack.domain.SexualAssaultKit;
import gov.idaho.isp.saktrack.domain.SexualAssaultKitRepository;
import gov.idaho.isp.saktrack.domain.audit.KitAudit;
import gov.idaho.isp.saktrack.domain.audit.KitAuditRepository;
import gov.idaho.isp.saktrack.domain.jurisdiction.Jurisdiction;
import gov.idaho.isp.saktrack.domain.organization.Organization;
import gov.idaho.isp.saktrack.domain.organization.OrganizationType;
import gov.idaho.isp.saktrack.domain.user.AdminUser;
import gov.idaho.isp.saktrack.domain.user.organization.LabUser;
import java.time.LocalDate;
import org.junit.Assert;
import org.junit.Test;
import static org.mockito.AdditionalAnswers.returnsFirstArg;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class AuditServiceTest {
  private final SexualAssaultKitRepository sakRepo = mock(SexualAssaultKitRepository.class);
  private final KitAuditRepository auditRepo = mock(KitAuditRepository.class);
  private final AuditService auditService;

  public AuditServiceTest() {
    auditService = new AuditServiceImpl(sakRepo, auditRepo, "MM/dd/yyyy");
  }

  @Test
  public void ChainOfCustodyEventAddedTest() {
    Jurisdiction jurisdiction = MockObjUtils.createJurisdiction();
    AdminUser user = MockObjUtils.createAdminUser();
    Organization labOrg = MockObjUtils.createOrganization(OrganizationType.LAB, jurisdiction);
    LabUser labUser = MockObjUtils.createOrganizationUser(labOrg);

    SexualAssaultKit oldKit = MockObjUtils.createSexualAssaultKit(labOrg);
    oldKit.setId(Long.valueOf(MockObjUtils.getUniqueId()));
    SexualAssaultKit newKit = (SexualAssaultKit)MockObjUtils.deepCopy(oldKit);
    newKit.addChainOfCustodyEvent(MockObjUtils.createChainOfCustodyEvent(ChainOfCustodyEvent.EventType.SEND, labUser, labOrg, labOrg));

    when(sakRepo.findOneInNewTransaction(any(Long.class))).thenReturn(oldKit);
    when(auditRepo.save(any(KitAudit.class))).then(returnsFirstArg());

    KitAudit audit = auditService.auditKit(newKit, "reason", user);

    Assert.assertEquals(1, audit.getChanges().size());
    Assert.assertTrue(audit.getChanges().get(0).contains("was added"));
  }

  @Test
  public void ChainOfCustodyEventRemovedTest() {
    Jurisdiction jurisdiction = MockObjUtils.createJurisdiction();
    AdminUser user = MockObjUtils.createAdminUser();
    Organization labOrg = MockObjUtils.createOrganization(OrganizationType.LAB, jurisdiction);
    LabUser labUser = MockObjUtils.createOrganizationUser(labOrg);

    SexualAssaultKit oldKit = MockObjUtils.createSexualAssaultKit(labOrg);
    oldKit.setId(Long.valueOf(MockObjUtils.getUniqueId()));
    SexualAssaultKit newKit = (SexualAssaultKit)MockObjUtils.deepCopy(oldKit);
    oldKit.addChainOfCustodyEvent(MockObjUtils.createChainOfCustodyEvent(ChainOfCustodyEvent.EventType.SEND, labUser, labOrg, labOrg));

    when(sakRepo.findOneInNewTransaction(any(Long.class))).thenReturn(oldKit);
    when(auditRepo.save(any(KitAudit.class))).then(returnsFirstArg());

    KitAudit audit = auditService.auditKit(newKit, "reason", user);

    Assert.assertEquals(1, audit.getChanges().size());
    Assert.assertTrue(audit.getChanges().get(0).contains("was removed"));
  }

  @Test
  public void ChainOfCustodyEventChangedTest() {
    Jurisdiction jurisdiction = MockObjUtils.createJurisdiction();
    AdminUser user = MockObjUtils.createAdminUser();
    Organization labOrg = MockObjUtils.createOrganization(OrganizationType.LAB, jurisdiction);
    LabUser labUser = MockObjUtils.createOrganizationUser(labOrg);

    SexualAssaultKit oldKit = MockObjUtils.createSexualAssaultKit(labOrg);
    oldKit.setId(Long.valueOf(MockObjUtils.getUniqueId()));
    oldKit.addChainOfCustodyEvent(MockObjUtils.createChainOfCustodyEvent(ChainOfCustodyEvent.EventType.SEND, labUser, labOrg, labOrg));
    SexualAssaultKit newKit = (SexualAssaultKit)MockObjUtils.deepCopy(oldKit);
    newKit.getChainOfCustody().get(0).setEventDate(LocalDate.now().minusDays(1));

    when(sakRepo.findOneInNewTransaction(any(Long.class))).thenReturn(oldKit);
    when(auditRepo.save(any(KitAudit.class))).then(returnsFirstArg());

    KitAudit audit = auditService.auditKit(newKit, "reason", user);

    Assert.assertEquals(1, audit.getChanges().size());
    Assert.assertTrue(audit.getChanges().get(0).contains("was change to"));
  }

  @Test
  public void EverythingChangedTest() {
    Jurisdiction jurisdiction = MockObjUtils.createJurisdiction();
    SexualAssaultKit oldKit = new SexualAssaultKit();
    oldKit.setId(Long.valueOf(MockObjUtils.getUniqueId()));
    oldKit.setExpirationDate(LocalDate.now().plusYears(2));

    SexualAssaultKit newKit = MockObjUtils.createSexualAssaultKit(MockObjUtils.createOrganization(OrganizationType.LAW_ENFORCEMENT, jurisdiction));
    newKit.setId(oldKit.getId());
    newKit.setQuestionableEvents(true);

    when(sakRepo.findOneInNewTransaction(any(Long.class))).thenReturn(oldKit);
    when(auditRepo.save(any(KitAudit.class))).then(returnsFirstArg());

    KitAudit audit = auditService.auditKit(newKit, "reason", MockObjUtils.createAdminUser());

    Assert.assertEquals(24, audit.getChanges().size());
    Assert.assertTrue(audit.getChanges().stream().anyMatch(s -> s.contains("Serial Number")));
    Assert.assertTrue(audit.getChanges().stream().anyMatch(s -> s.contains("Current Assignment")));
    Assert.assertTrue(audit.getChanges().stream().anyMatch(s -> s.contains("Expiration Date")));
    Assert.assertTrue(audit.getChanges().stream().anyMatch(s -> s.contains("Current Assignment")));
    Assert.assertTrue(audit.getChanges().stream().anyMatch(s -> s.contains("Questionable Events")));
    Assert.assertTrue(audit.getChanges().stream().anyMatch(s -> s.contains("Victim Type")));
    Assert.assertTrue(audit.getChanges().stream().anyMatch(s -> s.contains("Collection Date")));
    Assert.assertTrue(audit.getChanges().stream().anyMatch(s -> s.contains("Requesting LE Agency (on medical details)")));
    Assert.assertTrue(audit.getChanges().stream().anyMatch(s -> s.contains("Agency Case Number")));
    Assert.assertTrue(audit.getChanges().stream().anyMatch(s -> s.contains("Investigator")));
    Assert.assertTrue(audit.getChanges().stream().anyMatch(s -> s.contains("Crime")));
    Assert.assertTrue(audit.getChanges().stream().anyMatch(s -> s.contains("Crime Date")));
    Assert.assertTrue(audit.getChanges().stream().anyMatch(s -> s.contains("Reviewing Prosecutor")));
    Assert.assertTrue(audit.getChanges().stream().anyMatch(s -> s.contains("Outsourced Lab")));
    Assert.assertTrue(audit.getChanges().stream().anyMatch(s -> s.contains("Planned Destruction Date")));
    Assert.assertTrue(audit.getChanges().stream().anyMatch(s -> s.contains("Meets Submission Criteria")));
    Assert.assertTrue(audit.getChanges().stream().anyMatch(s -> s.contains("Non-Submission Reason")));
    Assert.assertTrue(audit.getChanges().stream().anyMatch(s -> s.contains("Prosecutor Agrees")));
    Assert.assertTrue(audit.getChanges().stream().anyMatch(s -> s.contains("Prosecutor Notes")));
    Assert.assertTrue(audit.getChanges().stream().anyMatch(s -> s.contains("Requesting LE Agency (on lab details)")));
    Assert.assertTrue(audit.getChanges().stream().anyMatch(s -> s.contains("Lab Case Number")));
    Assert.assertTrue(audit.getChanges().stream().anyMatch(s -> s.contains("Date Completed")));
    Assert.assertTrue(audit.getChanges().stream().anyMatch(s -> s.contains("DNA Database Entry")));
    Assert.assertTrue(audit.getChanges().stream().anyMatch(s -> s.contains("DNA Database Hit Date")));
    Assert.assertTrue(audit.getChanges().stream().anyMatch(s -> s.contains("Expunged Date")));
  }
}
