// google maps interactions

// rescued/cleaned/moved to v3 (c) 2013, didier Belot
// from ?? for tobaccocontrolgrants.org

$(document).ready(function(){
	initialize();
        $("#who_region").change(function(){
            var options = '<option value=""></option>';
            $("#countries").html(options);
            $.getJSON("../../../../ajax.php",{mode: 'whoCountries',who_region: $(this).val()}, function(j){
                $("#countries").html('');
                for (var i = 0; i < j.length; i++) {
                    options += '<option value="' + j[i].optionValue + '">' + j[i].optionDisplay + '</option>';
                }
                $("#countries").html(options);
            })
        })
        $("#from_date").datepicker({ dateFormat: 'yy-mm-dd' });
        $("#to_date").datepicker({ dateFormat: 'yy-mm-dd' });
        $('#date_type').change( function() {
            if (this.value!='') {
                $("#from_date_row").show();
                $("#to_date_row").show();
            } else {
                $("#from_date_row").hide();
                $("#to_date_row").hide();
            }
        })
        $("#resetbtn").click(function() {
            window.location = window.location;
        })
});

    $.fn.clearForm = function() {
        return this.each(function() {
            var type = this.type, tag = this.tagName.toLowerCase();
            if (tag == 'form')
                return $(':input',this).clearForm();
            if (type == 'text' || type == 'password' || tag == 'textarea')
                this.value = '';
            else if (type == 'checkbox' || type == 'radio')
                this.checked = false;
            else if (tag == 'select')
              this.selectedIndex = -1;
        });
    };

    var map = null;
    //var geocoder = null;

    function initialize() {

	map = new google.maps.Map(
		document.getElementById('map_canvas'), {
		center: new google.maps.LatLng(20, 10),
		zoom: 1,
		mapTypeId: google.maps.MapTypeId.ROADMAP
		}
	);

	for(x=0; x < points.length; x++) {
		var point = points[x];
		showAddress(point[0],point[1],point[2],point[3],point[4]);
      	}
    }

	function showAddress(address,plat,plong,info,id) {
	    if (plat!='' && plong!='') {
		var bbLatLng = new google.maps.LatLng(plat,plong);

		var bbMarker = new google.maps.Marker({
		    icon: '/img/marker.png',
		    position: bbLatLng,
		    map: map
		});
		google.maps.event.addListener(bbMarker, 'click', function() {
		  //alert('You clicked the map.');
		    $(".grants").hide("slow");
		    $("#country"+id).show("slow");
		    $('form').clearForm();
		    $("#from_date_row").hide();
		    $("#to_date_row").hide();
		});

	    } else {
		 alert(address + " point not specified");
	    }
	}
