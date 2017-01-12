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
                       UIView *root = view;
                       root.backgroundColor = [UIColor redColor];
                       root.yoga.isEnabled = YES;
                       root.yoga.flexGrow = 1;
                       root.yoga.flexShrink = 1;
                       root.yoga.alignItems = YGAlignCenter;
                       root.yoga.justifyContent = YGJustifyCenter;


                   });
    self = [super initWithDOMNode:view];
    if (self)
    {
    }
    return self;
}
@end
