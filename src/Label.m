//
//  Label.m
//  proactive
//
//  Created by Matt Lilley on 9/01/17.
//  Copyright © 2017 Matt Lilley. All rights reserved.
//

#import "Label.h"
#import <YogaKit/YogaKit.h>
#import <Proscript/proscript2.h>
#import "PrologObject.h"

@implementation Label
-(id) init
{
    dispatch_sync(dispatch_get_main_queue(),
                       ^{
                           view = [[UILabel alloc] init];
                           view.lineBreakMode = NSLineBreakByWordWrapping;
                           view.numberOfLines = 0;
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
                          [view setText:[Prolog atomString:[o value]]];
                      });
    }
}

@end
