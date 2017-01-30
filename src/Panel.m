//
//  Panel.m
//  proactive
//
//  Created by Matt Lilley on 7/01/17.
//  Copyright © 2017 Matt Lilley. All rights reserved.
//

#import "Panel.h"
#import <YogaKit/YogaKit.h>

@implementation Panel
-(id) init
{
    dispatch_sync(dispatch_get_main_queue(),
                   ^{
                       view = [[UIView alloc] init];
                       view.backgroundColor = [UIColor whiteColor];
                   });
    self = [super initWithDOMNode:view];
    if (self)
    {
    }
    return self;
}

-(void)setProperties:(NSDictionary *)properties
{
    [super setProperties:properties];
}
@end
