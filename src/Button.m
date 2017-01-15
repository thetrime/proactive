//
//  Button.m
//  proactive
//
//  Created by Matt Lilley on 16/01/17.
//  Copyright © 2017 Matt Lilley. All rights reserved.
//

#import "Button.h"
#import "PrologObject.h"
#import <YogaKit/YogaKit.h>

@implementation Button

-(id) init
{
    dispatch_sync(dispatch_get_main_queue(),
                  ^{
                      view = [UIButton buttonWithType:UIButtonTypeRoundedRect];
                      view.yoga.isEnabled = YES;
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
    if (properties[@"label"] != nil)
    {
        PrologObject* o = properties[@"label"];
        dispatch_sync(dispatch_get_main_queue(),
                      ^{
                          [view setTitle:[Prolog atomString:[o value]] forState:UIControlStateNormal];
                      });
    }
}

@end
