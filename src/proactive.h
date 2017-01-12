//
//  proactive.h
//  proactive
//
//  Created by Matt Lilley on 13/01/17.
//  Copyright © 2017 Matt Lilley. All rights reserved.
//

#import <UIKit/UIKit.h>

//! Project version number for proactive.
FOUNDATION_EXPORT double proactiveVersionNumber;

//! Project version string for proactive.
FOUNDATION_EXPORT const unsigned char proactiveVersionString[];

// In this header, you should import all the public headers of your framework using statements like #import <proactive/PublicHeader.h>

int render(NSString* url, void* propSpec, NSString* rootElementId, UIView* container);
