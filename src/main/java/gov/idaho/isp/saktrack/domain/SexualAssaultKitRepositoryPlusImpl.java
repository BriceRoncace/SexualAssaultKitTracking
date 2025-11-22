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

package gov.idaho.isp.saktrack.domain;

import gov.idaho.isp.saktrack.domain.jurisdiction.Jurisdiction;
import gov.idaho.isp.saktrack.service.SerialNumberFormatter;
import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import jakarta.persistence.Query;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;

import java.util.List;
import java.util.Optional;

public class SexualAssaultKitRepositoryPlusImpl implements SexualAssaultKitRepositoryPlus {

  private SerialNumberFormatter serialNumberFormatter;
  @PersistenceContext private EntityManager em;

  private static final String SELECT = "select sak from SexualAssaultKit sak ";
  private static final String COUNT = "select count(sak.id) from SexualAssaultKit sak ";

  @Override
  public SexualAssaultKit findBySerialNumber(String serialNumber) {
    Query query = em.createQuery("from SexualAssaultKit kit where kit.serialNumber = ?1");
    query.setParameter(1, getFormattedSerialNumber(serialNumber));
    List<SexualAssaultKit> results = query.getResultList();
    return results.size() > 0 ? results.get(0) : null;
  }
  //New -------------------------------------------------------------------------------------------------
  @Override
  public Page<SexualAssaultKit> findUnusedByOrganization(Long orgId, Pageable pageable) {
    return new PageImpl(findKits(NEW_BY_ORG, orgId, pageable), pageable, findCount(orgId, NEW_BY_ORG));
  }
  private static final String NEW_BY_ORG =
    "join sak.chainOfCustody c "
    + "left join sak.medicalDetails medicalDetails "
    + "where "
    + "sak.currentAssignment.id = ?1 "
    + "and c.eventType not in ('SEND', 'REPURPOSE') "
    + "and medicalDetails.collectionDate is null "
    + "and INDEX(c) = (select max(INDEX(c2)) from SexualAssaultKit sak2 join sak2.chainOfCustody c2 where sak2.id = sak.id) ";

  //Incoming -------------------------------------------------------------------------------------------------
  @Override
  public Page<SexualAssaultKit> findIncomingByOrganization(Long orgId, Pageable pageable) {
    return new PageImpl(findKits(INCOMING_BY_ORG, orgId, pageable), pageable, findCount(orgId, INCOMING_BY_ORG));
  }
  private static final String INCOMING_BY_ORG =
    "join sak.chainOfCustody c "
    + "left join sak.labDetails labDetails "
    + "where "
    + "sak.currentAssignment.id = ?1 "
    + "and c.eventType = 'SEND' "
    + "and INDEX(c) = (select max(INDEX(c2)) from SexualAssaultKit sak2 join sak2.chainOfCustody c2 where sak2.id = sak.id) ";

  //InProcess -------------------------------------------------------------------------------------------------
  //In process Lab
  @Override
  public Page<SexualAssaultKit> findUsedByOrganization(Long orgId, Pageable pageable) {
    return new PageImpl(findKits(INPROCESS_BY_ORG, orgId, pageable), pageable, findCount(orgId, INPROCESS_BY_ORG));
  }
  private static final String INPROCESS_BY_ORG =
    "join sak.chainOfCustody c "
    + "left join sak.medicalDetails medicalDetails "
    + "left join sak.labDetails labDetails "
    + "where "
    + "sak.currentAssignment.id = ?1 "
    + "and c.eventType != 'SEND' "
    + "and medicalDetails.collectionDate is not null "
    + "and INDEX(c) = (select max(INDEX(c2)) from SexualAssaultKit sak2 join sak2.chainOfCustody c2 where sak2.id = sak.id) ";

  //In process medical
  @Override
  public Page<SexualAssaultKit> findReceivedByOrganization(Long orgId, Pageable pageable) {
    return new PageImpl(findKits(INPROCESS_MEDICAL_BY_ORG, orgId, pageable), pageable, findCount(orgId, INPROCESS_MEDICAL_BY_ORG));
  }
  private static final String INPROCESS_MEDICAL_BY_ORG =
    "join sak.chainOfCustody c "
    + "left join sak.medicalDetails medicalDetails "
    + "where "
    + "sak.currentAssignment.id = ?1 "
    + "and c.eventType = 'RECEIVE' "
    + "and INDEX(c) = (select max(INDEX(c2)) from SexualAssaultKit sak2 join sak2.chainOfCustody c2 where sak2.id = sak.id) ";

  //Pending Submission -------------------------------------------------------------------------------------------------
  @Override
  public Page<SexualAssaultKit> findPendingSubmissionByOrganization(Long orgId, Pageable pageable) {
    return new PageImpl(findKits(PENDING_BY_ORG, orgId, pageable), pageable, findCount(orgId, PENDING_BY_ORG));
  }
  private static final String PENDING_BY_ORG =
    "join sak.chainOfCustody c "
    + "left join sak.leDetails leDetails "
    + "where "
    + "sak.currentAssignment.id = ?1 "
    + "and c.eventType = 'RECEIVE' "
    + "and INDEX(c) = (select max(INDEX(c2)) from SexualAssaultKit sak2 join sak2.chainOfCustody c2 where sak2.id = sak.id) "
    + "and sak.id not in "
    + "  (select sak2.id from SexualAssaultKit sak2 left join sak2.leDetails leDetails left join sak2.labDetails labDetails left join sak2.legalDetails legalDetails "
    + "   where sak2.id = sak.id and labDetails.dateCompleted is not null or (leDetails.meetsSubmissionCriteria = false and legalDetails.prosecutorAgrees = true)) ";

  //At Lab -------------------------------------------------------------------------------------------------
  @Override
  public Page<SexualAssaultKit> findByInLabAndRequestedBy(Long orgId, Pageable pageable) {
    return new PageImpl(findKits(IN_LAB_REQUESTED_BY_ORG, orgId, pageable), pageable, findCount(orgId, IN_LAB_REQUESTED_BY_ORG));
  }
  private static final String IN_LAB_REQUESTED_BY_ORG =
    "join sak.chainOfCustody c "
    + "left join sak.labDetails labDetails "
    + "left join sak.leDetails leDetails "
    + "where "
    + "labDetails.requestingLeAgency.id = ?1 "
    + "and sak.currentAssignment.type = 'LAB' "
    + "and INDEX(c) = (select max(INDEX(c2)) from SexualAssaultKit sak2 join sak2.chainOfCustody c2 where sak2.id = sak.id) ";

  //Analyzed -------------------------------------------------------------------------------------------------
  @Override
  public Page<SexualAssaultKit> findAnalyzedByOrganization(Long orgId, Pageable pageable) {
    return new PageImpl(findKits(ANALYZED_BY_ORG, orgId, pageable), pageable, findCount(orgId, ANALYZED_BY_ORG));
  }
  private static final String ANALYZED_BY_ORG =
    "join sak.chainOfCustody c "
    + "left join sak.labDetails labDetails "
    + "left join sak.leDetails leDetails "
    + "where "
    + "sak.currentAssignment.id = ?1 "
    + "and c.eventType = 'RECEIVE' "
    + "and INDEX(c) = (select max(INDEX(c2)) from SexualAssaultKit sak2 join sak2.chainOfCustody c2 where sak2.id = sak.id) "
    + "and not (labDetails.dateCompleted is null) ";

  //Unsubmittable -------------------------------------------------------------------------------------------------
  @Override
  public Page<SexualAssaultKit> findUnsubmittableByOrganization(Long orgId, Pageable pageable) {
    return new PageImpl(findKits(UNSUBMITTABLE_BY_ORG, orgId, pageable), pageable, findCount(orgId, UNSUBMITTABLE_BY_ORG));
  }
  private static final String UNSUBMITTABLE_BY_ORG =
    "join sak.chainOfCustody c "
    + "left join sak.labDetails labDetails "
    + "left join sak.leDetails leDetails "
    + "left join sak.legalDetails legalDetails "
    + "where "
    + "sak.currentAssignment.id = ?1 "
    + "and c.eventType = 'RECEIVE' "
    + "and INDEX(c) = (select max(INDEX(c2)) from SexualAssaultKit sak2 join sak2.chainOfCustody c2 where sak2.id = sak.id) "
    + "and labDetails.dateCompleted is null "
    + "and (leDetails.meetsSubmissionCriteria = false and legalDetails.prosecutorAgrees = true) ";


  //Needs Legal Attention -------------------------------------------------------------------------------------------------
  @Override
  public Page<SexualAssaultKit> findByNeedsLegalAttention(Long orgId, Jurisdiction jurisdiction, Pageable pageable) {
    return new PageImpl(findKits(NEEDS_LEGAL_ATTENTION, orgId, pageable, Optional.of(jurisdiction)), pageable, findCount(orgId, NEEDS_LEGAL_ATTENTION, Optional.of(jurisdiction)));
  }
  private static final String NEEDS_LEGAL_ATTENTION =
    "left join sak.leDetails leDetails "
    + "left join sak.legalDetails legalDetails "
    + "left join sak.currentAssignment currentAssignment "
    + "where "
    + "(legalDetails.reviewingOrganization.id = ?1 or (sak.currentAssignment.jurisdiction = ?2 and sak.currentAssignment.type = 'LAW_ENFORCEMENT')) "
    + "and leDetails.meetsSubmissionCriteria = false "
    + "and legalDetails.releasedForReview is not null "
    + "and legalDetails.reviewFinalized is null ";

  private String buildOrderByClause(Pageable pageable) {
    Sort sort = pageable.getSort();

    Sort.Order serialNumberOrder = sort.getOrderFor("serialNumber");
    if (serialNumberOrder != null) {
      return "order by sak.serialNumber " + serialNumberOrder.getDirection();
    }

    Sort.Order caseNumberOrder = sort.getOrderFor("caseNumber");
    if (caseNumberOrder != null) {
      return "order by labDetails.caseNumber " + caseNumberOrder.getDirection();
    }

    Sort.Order eventDateOrder = sort.getOrderFor("eventDate");
    if (eventDateOrder != null) {
      return "order by c.eventDate " + eventDateOrder.getDirection();
    }

    Sort.Order actorOrder = sort.getOrderFor("actor");
    if (actorOrder != null) {
      return "order by c.actor " + actorOrder.getDirection();
    }

    Sort.Order lastModifiedOrder = sort.getOrderFor("lastModified");
    if (lastModifiedOrder != null) {
      return "order by sak.lastModified " + lastModifiedOrder.getDirection();
    }

    Sort.Order expirationDateOrder = sort.getOrderFor("expirationDate");
    if (expirationDateOrder != null) {
      return "order by sak.expirationDate " + expirationDateOrder.getDirection();
    }

    Sort.Order fromOrder = sort.getOrderFor("from");
    if (fromOrder != null) {
      return "order by c.from.name " + fromOrder.getDirection();
    }

    Sort.Order leCaseNumberOrder = sort.getOrderFor("leCaseNumber");
    if (leCaseNumberOrder != null) {
      return "order by leDetails.caseNumber " + leCaseNumberOrder.getDirection();
    }

    Sort.Order destinationOrder = sort.getOrderFor("destination");
    if (destinationOrder != null) {
      return "order by medicalDetails.requestingLeAgency " + destinationOrder.getDirection();
    }

    Sort.Order crimeDateOrder = sort.getOrderFor("crimeDate");
    if (crimeDateOrder != null) {
      return "order by leDetails.crimeDate " + crimeDateOrder.getDirection();
    }

    Sort.Order currentAssignmentOrder = sort.getOrderFor("currentAssignment");
    if (currentAssignmentOrder != null) {
      return "order by currentAssignment.name " + currentAssignmentOrder.getDirection();
    }

    Sort.Order nonSubReasonOrder = sort.getOrderFor("nonSubReason");
    if (nonSubReasonOrder != null) {
      return "order by legalDetails.nonSubmissionReason " + nonSubReasonOrder.getDirection();
    }
    return "";
  }

  private List<SexualAssaultKit> findKits(String queryString, Long orgId, Pageable pageable) {
    return findKits(queryString, orgId, pageable, Optional.empty());
  }

  private List<SexualAssaultKit> findKits(String queryString, Long orgId, Pageable pageable, Optional<Jurisdiction> jurisdiction) {
    Query query = em.createQuery(SELECT + queryString + buildOrderByClause(pageable));
    query.setMaxResults(pageable.getPageSize());
    Long offset = pageable.getOffset();
    query.setFirstResult(offset.intValue());
    query.setParameter(1, orgId);
    if (jurisdiction.isPresent()) {
      query.setParameter(2, jurisdiction.get());
    }
    return query.getResultList();
  }

  private long findCount(Long orgId, String queryString) {
    return findCount(orgId, queryString, Optional.empty());
  }

  private long findCount(Long orgId, String queryString, Optional<Jurisdiction> jurisdiction) {
    Query query = em.createQuery(COUNT + queryString);
    query.setParameter(1, orgId);
    if (jurisdiction.isPresent()) {
      query.setParameter(2, jurisdiction.get());
    }
    Number number = (Number) query.getSingleResult();
    return number.longValue();
  }

  private String getFormattedSerialNumber(String serialNumber) {
    if (serialNumberFormatter.isValid(serialNumber)) {
      return serialNumberFormatter.format(serialNumber);
    }

    return serialNumber;
  }

  @Autowired
  public void setSerialNumberFormatter(SerialNumberFormatter serialNumberFormatter) {
    this.serialNumberFormatter = serialNumberFormatter;
  }
}
