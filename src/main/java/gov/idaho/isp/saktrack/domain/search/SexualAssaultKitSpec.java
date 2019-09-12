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

package gov.idaho.isp.saktrack.domain.search;

import gov.idaho.isp.saktrack.domain.ChainOfCustodyEvent;
import gov.idaho.isp.saktrack.domain.ChainOfCustodyEvent_;
import gov.idaho.isp.saktrack.domain.EventFlag;
import gov.idaho.isp.saktrack.domain.LabDetails;
import gov.idaho.isp.saktrack.domain.LabDetails_;
import gov.idaho.isp.saktrack.domain.LawEnforcementDetails;
import gov.idaho.isp.saktrack.domain.LawEnforcementDetails_;
import gov.idaho.isp.saktrack.domain.LegalDetails;
import gov.idaho.isp.saktrack.domain.LegalDetails_;
import gov.idaho.isp.saktrack.domain.MedicalDetails;
import gov.idaho.isp.saktrack.domain.MedicalDetails_;
import gov.idaho.isp.saktrack.domain.SexualAssaultKit;
import gov.idaho.isp.saktrack.domain.SexualAssaultKit_;
import gov.idaho.isp.saktrack.domain.jurisdiction.Jurisdiction;
import gov.idaho.isp.saktrack.domain.jurisdiction.Jurisdiction_;
import gov.idaho.isp.saktrack.domain.organization.Organization;
import gov.idaho.isp.saktrack.domain.organization.Organization_;
import java.util.ArrayList;
import java.util.List;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Join;
import javax.persistence.criteria.JoinType;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;
import javax.persistence.criteria.Subquery;
import org.apache.commons.lang3.StringUtils;
import org.springframework.data.jpa.domain.Specification;

public class SexualAssaultKitSpec implements Specification<SexualAssaultKit> {
  private final SexualAssaultKitSearchCriteria criteria;

  public SexualAssaultKitSpec(SexualAssaultKitSearchCriteria criteria) {
    this.criteria = criteria;
  }

  @Override
  public Predicate toPredicate(Root<SexualAssaultKit> root, CriteriaQuery<?> cq, CriteriaBuilder cb) {
    Subquery<Long> sq = cq.subquery(Long.class);
    Root<SexualAssaultKit> subQueryRoot = sq.from(SexualAssaultKit.class);
    sq.select(subQueryRoot.get(SexualAssaultKit_.id));
    sq.distinct(true);
    sq.where(subQueryPredicate(subQueryRoot, cq, cb));
    return cb.in(root.get(SexualAssaultKit_.id)).value(sq);
  }

  private Predicate subQueryPredicate(Root<SexualAssaultKit> subQueryRoot, CriteriaQuery<?> cq, CriteriaBuilder cb) {
    List<Predicate> predicates = new ArrayList<>();

    if (StringUtils.isNotBlank(criteria.getSerialNumber())) {
      predicates.add(cb.like(cb.lower(subQueryRoot.get(SexualAssaultKit_.serialNumber)), StringUtils.lowerCase("%" + criteria.getSerialNumber()) + "%"));
    }
    if (criteria.getMissingEvents() != null) {
      if (Boolean.TRUE.equals(criteria.getMissingEvents())) {
        predicates.add(cb.isTrue(subQueryRoot.get(SexualAssaultKit_.questionableEvents)));
      }
      else {
        predicates.add(cb.isFalse(subQueryRoot.get(SexualAssaultKit_.questionableEvents)));
      }
    }

    predicates.addAll(getCurrentAssignmentPredicates(subQueryRoot, cb));
    predicates.addAll(getChainOfEventPredicates(subQueryRoot, cq, cb));
    predicates.addAll(getLabDetailsPredicates(subQueryRoot, cb));
    predicates.addAll(getLawEnforcementDetailsPredicates(subQueryRoot, cb));
    predicates.addAll(getMedicalAndLegalPredicates(subQueryRoot, cb));

    return andTogether(predicates, cb);
  }

  private List<Predicate> getCurrentAssignmentPredicates(Root<SexualAssaultKit> root, CriteriaBuilder cb) {
    List<Predicate> assignmentPredicates = new ArrayList<>();

    if (criteria.getCurrentAgencyId() != null) {
      assignmentPredicates.add(cb.equal(root.get(SexualAssaultKit_.currentAssignment), criteria.getCurrentAgencyId()));
    }

    if (criteria.getCurrentJurisdictionId() != null) {
      Join<SexualAssaultKit, Organization> org = root.join(SexualAssaultKit_.currentAssignment);
      Join<Organization, Jurisdiction> jurisdiction = org.join(Organization_.jurisdiction);
      assignmentPredicates.add(cb.equal(jurisdiction.get(Jurisdiction_.id), criteria.getCurrentJurisdictionId()));
    }

    return assignmentPredicates;
  }

  private List<Predicate> getChainOfEventPredicates(Root<SexualAssaultKit> root, CriteriaQuery<?> cq, CriteriaBuilder cb) {
    List<Predicate> eventPredicates = new ArrayList<>();

    if (shouldJoinToChainOfCustodyEvent(criteria)) {
      Join<SexualAssaultKit, ChainOfCustodyEvent> events  = root.join(SexualAssaultKit_.chainOfCustody);

      if (criteria.getEventType() != null) {
        eventPredicates.add(cb.equal(events.get(ChainOfCustodyEvent_.eventType), criteria.getEventType()));
      }

      if (criteria.getEventOrganization() != null) {
        eventPredicates.add(cb.or(cb.equal(events.get(ChainOfCustodyEvent_.from), criteria.getEventOrganization()),
                             cb.equal(events.get(ChainOfCustodyEvent_.to), criteria.getEventOrganization())));
      }

      if (!criteria.getEventOrganizations().isEmpty()) {
        eventPredicates.add(events.get(ChainOfCustodyEvent_.actorOrganization).in(criteria.getEventOrganizations()));
      }

      if (StringUtils.isNotBlank(criteria.getEventActor())) {
        eventPredicates.add(cb.like(cb.lower(events.get(ChainOfCustodyEvent_.actor)), StringUtils.lowerCase("%" + criteria.getEventActor()) + "%"));
      }

      if (criteria.getEventDate() != null && criteria.getEventDate().canBuildPredicate()) {
        eventPredicates.add(criteria.getEventDate().buildPredicate(cb, events.get(ChainOfCustodyEvent_.eventDate)));
      }

      if (criteria.getLeReceivedCollectedKit() != null) {
        if (Boolean.TRUE.equals(criteria.getLeReceivedCollectedKit())) {
          eventPredicates.add(cb.equal(events.get(ChainOfCustodyEvent_.eventFlag), EventFlag.LE_RECEIVED_COLLECTED_KIT));
        }
        else {
          eventPredicates.add(cb.not(cb.in(root.get(SexualAssaultKit_.id)).value(getCollectedKitsReceivedByLe(cq, cb))));
        }
      }
    }

    return eventPredicates;
  }

  private Subquery<Long> getCollectedKitsReceivedByLe(CriteriaQuery<?> cq, CriteriaBuilder cb) {
    Subquery<Long> subquery = cq.subquery(Long.class);
    Root<SexualAssaultKit> subRoot = subquery.from(SexualAssaultKit.class);
    subquery.select(subRoot.get(SexualAssaultKit_.id));

    Join<SexualAssaultKit, ChainOfCustodyEvent> events = subRoot.join(SexualAssaultKit_.chainOfCustody);

    subquery.where(cb.equal(events.get(ChainOfCustodyEvent_.eventFlag), EventFlag.LE_RECEIVED_COLLECTED_KIT));
    return subquery;
  }

  private List<Predicate> getLabDetailsPredicates(Root<SexualAssaultKit> root, CriteriaBuilder cb) {
    List<Predicate> labPredicates = new ArrayList<>();

    if (shouldJoinToLabDetails(criteria)) {
      Join<SexualAssaultKit, LabDetails> labDetails = root.join(SexualAssaultKit_.labDetails);

      if (StringUtils.isNotBlank(criteria.getLabCaseNumber())) {
        labPredicates.add(cb.like(cb.lower(labDetails.get(LabDetails_.caseNumber)), StringUtils.lowerCase(criteria.getLabCaseNumber()) + "%"));
      }

      if (criteria.getCompletedDate()!= null && criteria.getCompletedDate().canBuildPredicate()) {
        labPredicates.add(criteria.getCompletedDate().buildPredicate(cb, labDetails.get(LabDetails_.dateCompleted)));
      }

      if (Boolean.TRUE.equals(criteria.getCompletedDateIsNull())) {
        labPredicates.add(cb.isNull(labDetails.get(LabDetails_.dateCompleted)));
      }

      if (criteria.getDnaDatabaseEntry() != null) {
        labPredicates.add(cb.equal(labDetails.get(LabDetails_.dnaDatabaseEntry), criteria.getDnaDatabaseEntry()));
      }

      if (criteria.getDnaDatabaseHitDate()!= null && criteria.getDnaDatabaseHitDate().canBuildPredicate()) {
        labPredicates.add(criteria.getDnaDatabaseHitDate().buildPredicate(cb, labDetails.get(LabDetails_.dnaDatabaseHitDate)));
      }

      if (criteria.getExpungedDate()!= null && criteria.getExpungedDate().canBuildPredicate()) {
        labPredicates.add(criteria.getExpungedDate().buildPredicate(cb, labDetails.get(LabDetails_.expungedDate)));
      }
    }

    return labPredicates;
  }

  private List<Predicate> getLawEnforcementDetailsPredicates(Root<SexualAssaultKit> root, CriteriaBuilder cb) {
    List<Predicate> lePredicates = new ArrayList<>();

    if (shouldJoinToLawEnforcementDetails(criteria)) {
      Join<SexualAssaultKit, LawEnforcementDetails> leDetails = root.join(SexualAssaultKit_.leDetails);

      if (StringUtils.isNotBlank(criteria.getLeCaseNumber())) {
        lePredicates.add(cb.like(cb.lower(leDetails.get(LawEnforcementDetails_.caseNumber)), StringUtils.lowerCase(criteria.getLeCaseNumber()) + "%"));
      }

      if (StringUtils.isNotBlank(criteria.getInvestigator())) {
        lePredicates.add(cb.like(cb.lower(leDetails.get(LawEnforcementDetails_.investigator)), StringUtils.lowerCase(criteria.getInvestigator()) + "%"));
      }

      if (StringUtils.isNotBlank(criteria.getCrime())) {
        lePredicates.add(cb.like(cb.lower(leDetails.get(LawEnforcementDetails_.crime)), StringUtils.lowerCase("%" + criteria.getCrime()) + "%"));
      }

      if (criteria.getCrimeDate() != null && criteria.getCrimeDate().canBuildPredicate()) {
        lePredicates.add(criteria.getCrimeDate().buildPredicate(cb, leDetails.get(LawEnforcementDetails_.crimeDate)));
      }

      if (criteria.getPlannedDestructionDate() != null && criteria.getPlannedDestructionDate().canBuildPredicate()) {
        lePredicates.add(criteria.getPlannedDestructionDate().buildPredicate(cb, leDetails.get(LawEnforcementDetails_.plannedDestructionDate)));
      }

      if (criteria.getMeetsSubmissionCriteria() != null) {
        if (Boolean.TRUE.equals(criteria.getMeetsSubmissionCriteria())) {
          lePredicates.add(cb.isTrue(leDetails.get(LawEnforcementDetails_.meetsSubmissionCriteria)));
        }
        else {
          lePredicates.add(cb.isFalse(leDetails.get(LawEnforcementDetails_.meetsSubmissionCriteria)));
        }
      }

      if (criteria.getNonSubmissionReason() != null) {
        lePredicates.add(cb.equal(leDetails.get(LawEnforcementDetails_.nonSubmissionReason), criteria.getNonSubmissionReason()));
      }
    }

    return lePredicates;
  }

  private List<Predicate> getMedicalAndLegalPredicates(Root<SexualAssaultKit> root, CriteriaBuilder cb) {
    List<Predicate> predicates = new ArrayList<>();

    if (shouldJoinToMedicalDetails(criteria) || shouldJoinToLegalDetails(criteria) || shouldJoinToLegalAndMedical(criteria)) {
      Join<SexualAssaultKit, MedicalDetails> medicalDetails = null;
      Join<SexualAssaultKit, LegalDetails> legalDetails = null;

      if (shouldJoinToMedicalDetails(criteria)) {
        medicalDetails = root.join(SexualAssaultKit_.medicalDetails, JoinType.LEFT);
        predicates.addAll(getMedicalDetailsPredicates(root, medicalDetails, cb));
      }

      if (shouldJoinToLegalDetails(criteria)) {
        legalDetails = root.join(SexualAssaultKit_.legalDetails, JoinType.LEFT);
        predicates.addAll(getLegalDetailsPredicates(root, legalDetails, cb));
      }

      if (shouldJoinToLegalAndMedical(criteria)) {
        if (medicalDetails == null) {
          medicalDetails = root.join(SexualAssaultKit_.medicalDetails, JoinType.LEFT);
        }
        if (legalDetails == null) {
          legalDetails = root.join(SexualAssaultKit_.legalDetails, JoinType.LEFT);
        }
        predicates.addAll(getLegalAssignmentPredicates(medicalDetails, legalDetails, cb));
      }
    }

    return predicates;
  }


  private List<Predicate> getMedicalDetailsPredicates(Root<SexualAssaultKit> root, Join<SexualAssaultKit, MedicalDetails> medicalDetails, CriteriaBuilder cb) {
    List<Predicate> medicalPredicates = new ArrayList<>();

    if (criteria.getVictimType() != null) {
      medicalPredicates.add(cb.equal(medicalDetails.get(MedicalDetails_.victimType), criteria.getVictimType()));
    }

    if (criteria.getRequestingLeAgencyId() != null) {
      medicalPredicates.add(cb.equal(medicalDetails.get(MedicalDetails_.requestingLeAgency), criteria.getRequestingLeAgencyId()));
    }

    if (Boolean.TRUE.equals(criteria.getRequestingLeAgencyNotNull())) {
      medicalPredicates.add(cb.isNotNull(medicalDetails.get(MedicalDetails_.requestingLeAgency)));
    }

    if (criteria.getCollectedDate()!= null && criteria.getCollectedDate().canBuildPredicate()) {
      medicalPredicates.add(criteria.getCollectedDate().buildPredicate(cb, medicalDetails.get(MedicalDetails_.collectionDate)));
    }

    if (criteria.getHasCollectedDate() != null) {
      if (Boolean.TRUE.equals(criteria.getHasCollectedDate())) {
        medicalPredicates.add(cb.isNotNull(medicalDetails.get(MedicalDetails_.collectionDate)));
      }
      else {
        medicalPredicates.add(cb.or(
          cb.isNull(medicalDetails.get(MedicalDetails_.collectionDate)),
          cb.isNull(root.get(SexualAssaultKit_.medicalDetails))));
      }
    }

    return medicalPredicates;
  }

  private List<Predicate> getLegalDetailsPredicates(Root<SexualAssaultKit> root, Join<SexualAssaultKit, LegalDetails> legalDetails, CriteriaBuilder cb) {
    List<Predicate> legalPredicates = new ArrayList<>();

    if (criteria.getProsecutorAgrees() != null) {
      if (Boolean.TRUE.equals(criteria.getProsecutorAgrees())) {
        legalPredicates.add(cb.isTrue(legalDetails.get(LegalDetails_.prosecutorAgrees)));
      }
      else {
        legalPredicates.add(cb.isFalse(legalDetails.get(LegalDetails_.prosecutorAgrees)));
      }
    }

    return legalPredicates;
  }

  private List<Predicate> getLegalAssignmentPredicates(Join<SexualAssaultKit, MedicalDetails> medicalDetails, Join<SexualAssaultKit, LegalDetails> legalDetails, CriteriaBuilder cb) {
    List<Predicate> prosecutorPredicates = new ArrayList<>();

    if (criteria.getJurisdictionId() != null && criteria.getReviewingProsecutorOrganization() == null) {
      Join<MedicalDetails, Organization> requestingLeAgency  = medicalDetails.join(MedicalDetails_.requestingLeAgency);
      Join<Organization, Jurisdiction> jurisdiction = requestingLeAgency.join(Organization_.jurisdiction);
      prosecutorPredicates.add(cb.equal(jurisdiction.get(Jurisdiction_.id), criteria.getJurisdictionId()));
    }
    else if (criteria.getReviewingProsecutorOrganization() != null && criteria.getJurisdictionId() == null) {
      prosecutorPredicates.add(cb.equal(legalDetails.get(LegalDetails_.reviewingOrganization), criteria.getReviewingProsecutorOrganization()));
    }
    else if (criteria.getJurisdictionId() != null && criteria.getReviewingProsecutorOrganization() != null) {
      Join<MedicalDetails, Organization> requestingLeAgency  = medicalDetails.join(MedicalDetails_.requestingLeAgency);
      Join<Organization, Jurisdiction> jurisdiction = requestingLeAgency.join(Organization_.jurisdiction);
      prosecutorPredicates.add(cb.or(cb.equal(jurisdiction.get(Jurisdiction_.id), criteria.getJurisdictionId()), cb.equal(legalDetails.get(LegalDetails_.reviewingOrganization), criteria.getReviewingProsecutorOrganization())));
    }

    return prosecutorPredicates;
  }

  private boolean shouldJoinToChainOfCustodyEvent(SexualAssaultKitSearchCriteria criteria) {
    return
      criteria.getEventType() != null ||
      criteria.getEventOrganization() != null ||
      StringUtils.isNotBlank(criteria.getEventActor()) ||
      (criteria.getEventDate() != null && criteria.getEventDate().canBuildPredicate()) ||
      criteria.getLeReceivedCollectedKit() != null;
  }

  private boolean shouldJoinToLawEnforcementDetails(SexualAssaultKitSearchCriteria criteria) {
    return
      StringUtils.isNotBlank(criteria.getLeCaseNumber()) ||
      StringUtils.isNotBlank(criteria.getInvestigator()) ||
      StringUtils.isNotBlank(criteria.getCrime()) ||
      (criteria.getCrimeDate() != null && criteria.getCrimeDate().canBuildPredicate()) ||
      criteria.getMeetsSubmissionCriteria() != null ||
      criteria.getNonSubmissionReason() != null ||
      (criteria.getPlannedDestructionDate() != null && criteria.getPlannedDestructionDate().canBuildPredicate());
  }

  private boolean shouldJoinToLabDetails(SexualAssaultKitSearchCriteria criteria) {
    return
      StringUtils.isNotBlank(criteria.getLabCaseNumber()) ||
      (criteria.getCompletedDate()!= null && criteria.getCompletedDate().canBuildPredicate()) ||
      criteria.getDnaDatabaseEntry() != null ||
      (criteria.getDnaDatabaseHitDate()!= null && criteria.getDnaDatabaseHitDate().canBuildPredicate()) ||
      (criteria.getExpungedDate()!= null && criteria.getExpungedDate().canBuildPredicate());
  }

  private boolean shouldJoinToLegalAndMedical(SexualAssaultKitSearchCriteria criteria) {
    return
    criteria.getReviewingProsecutorOrganization() != null ||
    criteria.getJurisdictionId() != null;
  }

  private boolean shouldJoinToLegalDetails(SexualAssaultKitSearchCriteria criteria) {
    return criteria.getProsecutorAgrees() != null;
  }

  private boolean shouldJoinToMedicalDetails(SexualAssaultKitSearchCriteria criteria) {
    return
      criteria.getVictimType() != null ||
      criteria.getRequestingLeAgencyId() != null ||
      Boolean.TRUE.equals(criteria.getRequestingLeAgencyNotNull()) ||
      (criteria.getCollectedDate() != null && criteria.getCollectedDate().canBuildPredicate()) ||
      criteria.getHasCollectedDate() != null;
  }

  private Predicate andTogether(List<Predicate> predicates, CriteriaBuilder cb) {
    return cb.and(predicates.toArray(new Predicate[0]));
  }
}