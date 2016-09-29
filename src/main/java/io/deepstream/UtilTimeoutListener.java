package io.deepstream;

import com.google.j2objc.annotations.ObjectiveCName;

interface UtilTimeoutListener {
	@ObjectiveCName("onTimeout:action:event:name:")
    void onTimeout(Topic topic, Actions action, Event event, String name );
}
