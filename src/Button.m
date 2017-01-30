//
//  Button.m
//  proactive
//
//  Created by Matt Lilley on 16/01/17.
//  Copyright © 2017 Matt Lilley. All rights reserved.
//

#import "Button.h"
#import "PrologObject.h"
#import "Constants.h"
#import "ReactWidget.h"
#import <YogaKit/YogaKit.h>

@implementation Button

-(id) init
{
    dispatch_sync(dispatch_get_main_queue(),
                  ^{
                      view = [UIButton buttonWithType:UIButtonTypeRoundedRect];
                      [view addTarget:self action:@selector(onClick:) forControlEvents:UIControlEventTouchUpInside];
                      view.yoga.isEnabled = YES;
                      clickListener = -1;
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
    if (properties[@"onClick"] != nil)
    {
        PrologObject* o = properties[@"onClick"];
        if (clickListener != -1)
            [Prolog freeLocal:clickListener];
        if ([Constants isNull:[o value]])
            clickListener = -1;
        else
            clickListener = [Prolog makeLocal:[o value]];
    }
}

-(void)onClick:(UIButton*)button
{
    if (clickListener != -1)
        [owner triggerEvent:clickListener withData:[ReactComponent serialize:@{}] thenCall:^(int rc) {}];

}


@end
