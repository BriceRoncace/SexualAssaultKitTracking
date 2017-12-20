package gov.idaho.isp.saktrack.controller.lab;

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
public class LabDashboardController extends BaseOrganizationController {
  private final SexualAssaultKitRepository sexualAssaultKitRepository;

  public LabDashboardController(OrganizationUserRepository organizationUserRepository, SexualAssaultKitRepository sexualAssaultKitRepository) {
    super(organizationUserRepository);
    this.sexualAssaultKitRepository = sexualAssaultKitRepository;
  }

  @RequestMapping(value = "/lab/dashboard", method = RequestMethod.GET)
  public String dashboard(@CookieValue Optional<String> labPagingParams,
                          @RequestAttribute Organization organization,
                          LabDashboardPageRequest pageRequests,
                          Model model,
                          @RequestAttribute User user,
                          @RequestParam Map<String,String> requestParams,
                          HttpServletResponse response) throws UnsupportedEncodingException {

    if (requestParams.isEmpty() && labPagingParams.isPresent()) {
      return "redirect:/lab/dashboard" + labPagingParams.get();
    }
    model.addAttribute("organization", organization);

    model.addAttribute("newKits", findNewKits(organization, pageRequests.getNew()));
    model.addAttribute("incomingKits", findIncomingKits(organization, pageRequests.getIncoming()));
    model.addAttribute("inProcessKits", findInProcessKits(organization, pageRequests.getInProcess()));

    if (canAndShouldSeeUnverifiedUserMessage(organization, user)) {
      model.addAttribute("alerts", getText("new.user.verification"));
    }

    response.addCookie(CookieUtils.createEncoded("labPagingParams", pageRequests.getAsUrlParams()));
    return "lab/dashboard";
  }

  private Page<SexualAssaultKit> findNewKits(Organization org, PageRequest pageReq) {
    Function<Pageable,Page<SexualAssaultKit>> getPageFn = pr -> sexualAssaultKitRepository.findUnusedByOrganization(org.getId(), pr);
    return PagingUtils.getFirstPageIfRequestedIsBlank(getPageFn, pageReq);
  }

  private Page<SexualAssaultKit> findIncomingKits(Organization org, PageRequest pageReq) {
    Function<Pageable,Page<SexualAssaultKit>> getPageFn = pr -> sexualAssaultKitRepository.findIncomingByOrganization(org.getId(), pr);
    return PagingUtils.getFirstPageIfRequestedIsBlank(getPageFn, pageReq);
  }

  private Page<SexualAssaultKit> findInProcessKits(Organization org, PageRequest pageReq) {
    Function<Pageable,Page<SexualAssaultKit>> getPageFn = pr -> sexualAssaultKitRepository.findUsedByOrganization(org.getId(), pr);
    return PagingUtils.getFirstPageIfRequestedIsBlank(getPageFn, pageReq);
  }

  public static class LabDashboardPageRequest {
    private static final int DEFAULT_PAGE_SIZE = 100;

    private int newPage;
    private SortWrapper newSort;

    private int incomingPage;
    private SortWrapper incomingSort;

    private int inProcessPage;
    private SortWrapper inProcessSort;

    public int getNewPage() {
      return newPage;
    }

    public void setNewPage(int newPage) {
      this.newPage = newPage;
    }

    public SortWrapper getNewSort() {
      return newSort;
    }

    public void setNewSort(SortWrapper newSort) {
      this.newSort = newSort;
    }

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

    public PageRequest getNew() {
      if (newSort != null) {
        return new PageRequest(newPage, DEFAULT_PAGE_SIZE, newSort.unwrap());
      }
      return new PageRequest(newPage, DEFAULT_PAGE_SIZE);
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

    public String getAsUrlParams() {
      StringBuilder sb = new StringBuilder();
      sb.append("?newPage=").append(newPage)
        .append("&newSort=").append(PagingUtils.getSingleOrderBy(newSort))
        .append("&incomingPage=").append(incomingPage)
        .append("&incomingSort=").append(PagingUtils.getSingleOrderBy(incomingSort))
        .append("&inProcessPage=").append(inProcessPage)
        .append("&inProcessSort=").append(PagingUtils.getSingleOrderBy(inProcessSort));
      return sb.toString();
    }
  }
}
