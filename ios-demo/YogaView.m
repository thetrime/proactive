//
//  YogaView.m
//  proactive
//
//  Created by Matt Lilley on 15/01/17.
//  Copyright © 2017 Matt Lilley. All rights reserved.
//

#import "YogaView.h"
#import <YogaKit/YogaKit.h>

@implementation YogaView

/*
// Only override drawRect: if you perform custom drawing.
// An empty implementation adversely affects performance during animation.
- (void)drawRect:(CGRect)rect {
    // Drawing code
}
*/

-(void)didAddSubview:(UIView *)subview
{
    if (self.yoga.isEnabled)
        [self.yoga applyLayout];
}

@end
