package gov.idaho.isp.saktrack.controller.lawenforcement;

import gov.idaho.isp.saktrack.SexualAssaultKit;
import gov.idaho.isp.saktrack.controller.BaseOrganizationController;
import gov.idaho.isp.saktrack.organization.Organization;
import gov.idaho.isp.saktrack.persistence.SexualAssaultKitRepository;
import gov.idaho.isp.saktrack.user.User;
import gov.idaho.isp.saktrack.user.persistence.OrganizationUserRepository;
import gov.idaho.isp.saktrack.util.CookieUtils;
import gov.idaho.isp.saktrack.util.PagingUtils;
import gov.idaho.isp.saktrack.util.SortWrapper;
import java.io.UnsupportedEncodingException;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;
import javax.servlet.http.HttpServletResponse;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.CookieValue;
import org.springframework.web.bind.annotation.RequestAttribute;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

@Controller
public class LawEnforcementDashboardController extends BaseOrganizationController {
  private final SexualAssaultKitRepository sexualAssaultKitRepository;

  public LawEnforcementDashboardController(OrganizationUserRepository organizationUserRepository, SexualAssaultKitRepository sexualAssaultKitRepository) {
    super(organizationUserRepository);
    this.sexualAssaultKitRepository = sexualAssaultKitRepository;
  }

  @RequestMapping(value = "/law-enforcement/dashboard", method = RequestMethod.GET)
  public String dashboard(@CookieValue Optional<String> lePagingParams,
                          @RequestAttribute Organization organization,
                          LeDashboardPageRequest pageRequests,
                          Model model,
                          @RequestAttribute User user,
                          @RequestParam Map<String,String> requestParams,
                          HttpServletResponse response) throws UnsupportedEncodingException {

    if (requestParams.isEmpty() && lePagingParams.isPresent()) {
      return "redirect:/law-enforcement/dashboard" + lePagingParams.get();
    }

    model.addAttribute("organization", organization);

    model.addAttribute("incomingKits", findIncomingKits(organization, pageRequests.getIncoming()));
    model.addAttribute("inProcessKits", findInProcessKits(organization, pageRequests.getInProcess()));
    model.addAttribute("kitsInLab", findKitsInLab(organization, pageRequests.getInLab()));
    model.addAttribute("analyzedKits", findAnalyzedKits(organization, pageRequests.getAnalyzed()));
    model.addAttribute("unsubmittableKits", findUnsubmittableKits(organization, pageRequests.getUnsubmittable()));

    if (canAndShouldSeeUnverifiedUserMessage(organization, user)) {
      model.addAttribute("alerts", getText("new.user.verification"));
    }

    response.addCookie(CookieUtils.createEncoded("lePagingParams", pageRequests.getAsUrlParams()));
    return "law-enforcement/dashboard";
  }

  private Page<SexualAssaultKit> findIncomingKits(Organization org, PageRequest pageReq) {
    Function<Pageable,Page<SexualAssaultKit>> getPageFn = pr -> sexualAssaultKitRepository.findIncomingByOrganization(org.getId(), pr);
    return PagingUtils.getFirstPageIfRequestedIsBlank(getPageFn, pageReq);
  }

  private Page<SexualAssaultKit> findInProcessKits(Organization org, PageRequest pageReq) {
    Function<Pageable,Page<SexualAssaultKit>> getPageFn = pr -> sexualAssaultKitRepository.findPendingSubmissionByOrganization(org.getId(), pr);
    return PagingUtils.getFirstPageIfRequestedIsBlank(getPageFn, pageReq);
  }

  private Page<SexualAssaultKit> findKitsInLab(Organization org, PageRequest pageReq) {
    Function<Pageable,Page<SexualAssaultKit>> getPageFn = pr -> sexualAssaultKitRepository.findByInLabAndRequestedBy(org.getId(), pr);
    return PagingUtils.getFirstPageIfRequestedIsBlank(getPageFn, pageReq);
  }

  private Page<SexualAssaultKit> findAnalyzedKits(Organization org, PageRequest pageReq) {
    Function<Pageable,Page<SexualAssaultKit>> getPageFn = pr -> sexualAssaultKitRepository.findAnalyzedByOrganization(org.getId(), pr);
    return PagingUtils.getFirstPageIfRequestedIsBlank(getPageFn, pageReq);
  }

  private Page<SexualAssaultKit> findUnsubmittableKits(Organization org, PageRequest pageReq) {
    Function<Pageable,Page<SexualAssaultKit>> getPageFn = pr -> sexualAssaultKitRepository.findUnsubmittableByOrganization(org.getId(), pr);
    return PagingUtils.getFirstPageIfRequestedIsBlank(getPageFn, pageReq);
  }

  public static class LeDashboardPageRequest {
    private static final int DEFAULT_PAGE_SIZE = 20;

    private int incomingPage;
    private SortWrapper incomingSort;
    private int inProcessPage;
    private SortWrapper inProcessSort;
    private int inLabPage;
    private SortWrapper inLabSort;
    private int analyzedPage;
    private SortWrapper analyzedSort;
    private int unsubmittablePage;
    private SortWrapper unsubmittableSort;

    public int getIncomingPage() {
      return incomingPage;
    }

    public void setIncomingPage(int incomingPage) {
      this.incomingPage = incomingPage;
    }

    public SortWrapper getIncomingSort() {
      return incomingSort;
    }

    public void setIncomingSort(SortWrapper incomingSort) {
      this.incomingSort = incomingSort;
    }

    public int getInProcessPage() {
      return inProcessPage;
    }

    public void setInProcessPage(int inProcessPage) {
      this.inProcessPage = inProcessPage;
    }

    public SortWrapper getInProcessSort() {
      return inProcessSort;
    }

    public void setInProcessSort(SortWrapper inProcessSort) {
      this.inProcessSort = inProcessSort;
    }

    public int getInLabPage() {
      return inLabPage;
    }

    public void setInLabPage(int inLabPage) {
      this.inLabPage = inLabPage;
    }

    public SortWrapper getInLabSort() {
      return inLabSort;
    }

    public void setInLabSort(SortWrapper inLabSort) {
      this.inLabSort = inLabSort;
    }

    public int getAnalyzedPage() {
      return analyzedPage;
    }

    public void setAnalyzedPage(int analyzedPage) {
      this.analyzedPage = analyzedPage;
    }

    public SortWrapper getAnalyzedSort() {
      return analyzedSort;
    }

    public void setAnalyzedSort(SortWrapper analyzedSort) {
      this.analyzedSort = analyzedSort;
    }

    public int getUnsubmittablePage() {
      return unsubmittablePage;
    }

    public void setUnsubmittablePage(int unsubmittablePage) {
      this.unsubmittablePage = unsubmittablePage;
    }

    public SortWrapper getUnsubmittableSort() {
      return unsubmittableSort;
    }

    public void setUnsubmittableSort(SortWrapper unsubmittableSort) {
      this.unsubmittableSort = unsubmittableSort;
    }

    public PageRequest getIncoming() {
      if (incomingSort != null) {
        return new PageRequest(incomingPage, DEFAULT_PAGE_SIZE, incomingSort.unwrap());
      }
      return new PageRequest(incomingPage, DEFAULT_PAGE_SIZE);
    }

    public PageRequest getInProcess() {
      if (inProcessSort != null) {
        return new PageRequest(inProcessPage, DEFAULT_PAGE_SIZE, inProcessSort.unwrap());
      }
      return new PageRequest(inProcessPage, DEFAULT_PAGE_SIZE);
    }

    public PageRequest getInLab() {
      if (inLabSort != null) {
        return new PageRequest(inLabPage, DEFAULT_PAGE_SIZE, inLabSort.unwrap());
      }
      return new PageRequest(inLabPage, DEFAULT_PAGE_SIZE);
    }

    public PageRequest getAnalyzed() {
      if (analyzedSort != null) {
        return new PageRequest(analyzedPage, DEFAULT_PAGE_SIZE, analyzedSort.unwrap());
      }
      return new PageRequest(analyzedPage, DEFAULT_PAGE_SIZE);
    }

    public PageRequest getUnsubmittable() {
      if (unsubmittableSort != null) {
        return new PageRequest(unsubmittablePage, DEFAULT_PAGE_SIZE, unsubmittableSort.unwrap());
      }
      return new PageRequest(unsubmittablePage, DEFAULT_PAGE_SIZE);
    }

    public String getAsUrlParams() {
      StringBuilder sb = new StringBuilder();
      sb.append("?incomingPage=").append(incomingPage)
        .append("&incomingSort=").append(PagingUtils.getSingleOrderBy(incomingSort))
        .append("&inProcessPage=").append(inProcessPage)
        .append("&inProcessSort=").append(PagingUtils.getSingleOrderBy(inProcessSort))
        .append("&inLabPage=").append(inLabPage)
        .append("&inLabSort=").append(PagingUtils.getSingleOrderBy(inLabSort))
        .append("&analyzedPage=").append(analyzedPage)
        .append("&analyzedSort=").append(PagingUtils.getSingleOrderBy(analyzedSort))
        .append("&unsubmittablePage=").append(unsubmittablePage)
        .append("&unsubmittableSort=").append(PagingUtils.getSingleOrderBy(unsubmittableSort));
      return sb.toString();
    }
  }
}
