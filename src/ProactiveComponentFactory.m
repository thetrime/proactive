//
//  ProactiveComponentFactory.m
//  proactive
//
//  Created by Matt Lilley on 7/01/17.
//  Copyright © 2017 Matt Lilley. All rights reserved.
//

#import "ProactiveComponentFactory.h"
#import "Panel.h"
#import "Label.h"

@implementation ProactiveComponentFactory
+(ReactComponent*)createElementOfType:(NSString*)type withContext:(ReactComponent*)context
{
    if ([type isEqualToString:@"Panel"])
        return [[Panel alloc] init];
    if ([type isEqualToString:@"Label"])
        return [[Label alloc] init];

    NSLog(@"Not implemented: %@", type);
    assert(0);
    return nil;
}
@end
