
// Disable / enable a button
Shiny.addCustomMessageHandler('togglewidgetSFM', function(data) {
  if (data.type == 'disable') {
    $('#' + data.inputId).attr("disabled", true);
    $('#' + data.inputId).addClass('disabled');
  }
  if (data.type == 'enable') {
    $('#' + data.inputId).attr("disabled", false);
    $('#' + data.inputId).removeClass('disabled');
  }
});


Shiny.addCustomMessageHandler('unbindDTSFM', function(data) {
  table = $('#' + data.id).find('table');
  if(table.length > 0){
      Shiny.unbindAll(table.DataTable().table().node());
  }
});
