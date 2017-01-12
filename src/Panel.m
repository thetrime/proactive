//
//  Panel.m
//  proactive
//
//  Created by Matt Lilley on 7/01/17.
//  Copyright © 2017 Matt Lilley. All rights reserved.
//

#import "Panel.h"

@implementation Panel
-(id) init
{
    view = [[UIView alloc] init];
    view.frame = CGRectMake(0, 0, 200, 200);
    [view setBackgroundColor:[UIColor greenColor]];
    self = [super initWithDOMNode:view];
    if (self)
    {
    }
    return self;
}
@end
