//
//  ProactiveComponentFactory.h
//  proactive
//
//  Created by Matt Lilley on 7/01/17.
//  Copyright © 2017 Matt Lilley. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "ReactComponent.h"

@interface ProactiveComponentFactory : NSObject
+(ReactComponent*)createElementOfType:(NSString*)type withContext:(ReactComponent*)context;
@end
