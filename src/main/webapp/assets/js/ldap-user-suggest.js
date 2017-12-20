var ldapUserSuggest = {
  selection: null,
  $typeahead: null,

  setup: function(options) {
    var ldapUsers = new Bloodhound({
      datumTokenizer: Bloodhound.tokenizers.obj.whitespace('value'),
      queryTokenizer: Bloodhound.tokenizers.whitespace,
      remote: { url: options.url, wildcard: '%QUERY' }
    });

    ldapUserSuggest.$typeahead = $('[data-ldap-user-suggest]').typeahead({
      minLength: 3,
      highlight: true
    }, 
    { 
      display: 'fullName',
      source: ldapUsers,
      limit: 15
    }).on('typeahead:selected typeahead:autocomplete typeahead:render', function(e, selection) {
      ldapUserSuggest.selection = selection;
      if (selection && options.onSelect) {
        options.onSelect(selection);
      }
    }).on('typeahead:asyncrequest', function() {
      $(this).addClass("ajax-loading");
    }).on('typeahead:asyncreceive', function() {
      $(this).removeClass("ajax-loading");
    });
  },
  
  clear: function() {
    ldapUserSuggest.selection = null;
    ldapUserSuggest.$typeahead.typeahead('val', '');
  }
};