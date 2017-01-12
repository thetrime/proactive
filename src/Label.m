//
//  Label.m
//  proactive
//
//  Created by Matt Lilley on 9/01/17.
//  Copyright © 2017 Matt Lilley. All rights reserved.
//

#import "Label.h"

@implementation Label
-(id) init
{
    view = [UILabel alloc];
    dispatch_async(dispatch_get_main_queue(),
                   ^{
                       view = [view init];
                       view.frame = CGRectMake(0, 0, 100, 100);
                       [view setText:@"Hello, world"];
                   });
    self = [super initWithDOMNode:view];
    if (self)
    {
        
    }
    return self;
}

@end
