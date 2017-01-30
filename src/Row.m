//
//  Row.m
//  proactive
//
//  Created by Matt Lilley on 23/01/17.
//  Copyright © 2017 Matt Lilley. All rights reserved.
//

#import "Row.h"
#import <YogaKit/YogaKit.h>

@implementation Row
-(id) init
{
    dispatch_sync(dispatch_get_main_queue(),
                  ^{
                      view = [[UITableViewCell alloc] init];
                      view.contentView.yoga.isEnabled = YES;
//                      view.contentView.yoga.height = 100;
//                      view.contentView.yoga.width = 300;
                      view.contentView.yoga.paddingLeft = view.layoutMargins.left;
                  });
    self = [super initWithDOMNode:view.contentView];
    if (self)
    {
    }
    return self;
}

-(UITableViewCell*)cell
{
    [view.contentView.yoga applyLayout];
    return view;
}

-(void)setProperties:(NSDictionary *)properties
{
    [super setProperties:properties];
    // TBD: onDblClick
}

@end
