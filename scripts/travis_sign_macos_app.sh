#!/usr/bin/env sh

# secure/hidden variable from travis
#CERTIFICATE_ID="..."
#USERNAME="..."
#APP_SPECIFIC_PASSWORD="..." 
#RELEASE_ZIP_FILE="..."

# path to app
FILE_APP='enduser/macos/app/trackereditor.app'
PLISTBUDDY_APP='/usr/libexec/PlistBuddy'

if [ ! -x "${PLISTBUDDY_APP}" ]
then
	echo "Couldn't find PlistBuddy"
	exit 1
fi

# copy everything into enduser/macos/app folder
#
# Move the executable to the application bundle
mv enduser/trackereditor enduser/macos/app/trackereditor.app/Contents/MacOS

# Move the trackers list to application bundle
mv enduser/add_trackers.txt enduser/macos/app/trackereditor.app/Contents/MacOS
mv enduser/remove_trackers.txt enduser/macos/app/trackereditor.app/Contents/MacOS

# move all the *.txt file
mv enduser/*.txt enduser/macos/app

# sign the app. -sign is the developer cetificate ID
# entitlements does not work at this moment
#codesign --timestamp --entitlements enduser/macos/entitlements.plist --force --options runtime --deep --sign $CERTIFICATE_ID $FILE_APP
codesign --timestamp --force --options runtime --deep --sign $CERTIFICATE_ID $FILE_APP

# Check exit code
exit_code=$?
if [ "${exit_code}" != "0" ]
then
	echo "codesign failed: ${exit_code}"
	exit 1
fi

#must use ditto to compress the file application folder only for notarize. Zip program will not work!
/usr/bin/ditto -c -k --keepParent "$FILE_APP" "$RELEASE_ZIP_FILE"

# upload zip to notarize service. for RequestUUID 
# -- username is the normal apple ID. example myname@mail.com 
# -- password is 'app specific password' generated via apple web site. Security -> app-specific password
echo "Uploading to notarize server" 
xcrun altool --notarize-app --output-format xml --primary-bundle-id "trackereditor" --username $USERNAME --password $APP_SPECIFIC_PASSWORD --file $RELEASE_ZIP_FILE > "result.plist"

# remove the uploaded zip file. need to be created again later.
rm $RELEASE_ZIP_FILE 


# Check exit code
exit_code=$?
if [ "${exit_code}" != "0" ]
then
	echo "notarize-app failed: ${exit_code}"
	cat "result.plist"
	exit 1
fi

# Get the RequestUUID
RequestUUID="$("${PLISTBUDDY_APP}" -c "Print notarization-upload:RequestUUID"  "result.plist")"
echo "RequestUUID: ${RequestUUID}"

# wait till notarize apple server is finish with the processing the zip upload.
for (( ; ; ))
do

    # get status from apple notarize server
    xcrun altool --output-format xml --notarization-info "${RequestUUID}"  -u $USERNAME -p $APP_SPECIFIC_PASSWORD > "result.plist"
    # Check exit code
    exit_code=$?
    if [ "${exit_code}" != "0" ]
    then
    	echo "notarization-info failed: ${exit_code}"
	    cat "result.plist"
        # print the error in the URL
        LogFileURL="$("${PLISTBUDDY_APP}" -c "Print notarization-info:LogFileURL"  "result.plist")"
        if [ ! -z "${LogFileURL}" ]
        then
            curl "${LogFileURL}"
        fi
	    exit 1
    fi

    # get the status. 
    StatusCode="$("${PLISTBUDDY_APP}" -c "Print notarization-info:Status"  "result.plist")"
    echo "Status: ${StatusCode}"

    # if no status code present in result then it is still busy
    if [ "${StatusCode}" == "in progress" ]
    then
        sleep 15
    else
   	    echo "Finish waiting."
        #cat "result.plist"   
        # Check if everything is correct 
        StatusCode="$("${PLISTBUDDY_APP}" -c "Print notarization-info:'Status Code'"  "result.plist")"
        echo "Status code: ${StatusCode}"
        if [ "${StatusCode}" == "0" ]
        then
            # there are no error.
            break
        else
            # print the error in the URL
            LogFileURL="$("${PLISTBUDDY_APP}" -c "Print notarization-info:LogFileURL"  "result.plist")"
            if [ ! -z "${LogFileURL}" ]
            then
                curl "${LogFileURL}"
            fi
	    exit 1
        fi
    fi
done

# verify sign *.app is succes full
spctl --assess --type execute --verbose $FILE_APP
# Check exit code
exit_code=$?
if [ "${exit_code}" != "0" ]
then
	echo "spctl failed: ${exit_code}"
	exit 1
fi

# staple the *.app
xcrun stapler staple $FILE_APP
# Check exit code
exit_code=$?
if [ "${exit_code}" != "0" ]
then
	echo "spctl stapler: ${exit_code}"
	exit 1
fi

# zip only the app folder with extra text file.
/usr/bin/ditto -c -k "enduser/macos/app" "$RELEASE_ZIP_FILE"
