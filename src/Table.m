//
//  Table.m
//  proactive
//
//  Created by Matt Lilley on 23/01/17.
//  Copyright © 2017 Matt Lilley. All rights reserved.
//

#import "Table.h"
#import "PrologObject.h"
#import "Row.h"
#import <YogaKit/YogaKit.h>

@implementation Table

-(id) init
{
    dispatch_sync(dispatch_get_main_queue(),
                  ^{
                      view = [[UITableView alloc] init];
                      view.dataSource = self;
                      view.yoga.isEnabled = YES;
                      view.tableFooterView = [[UIView alloc] initWithFrame:CGRectZero];
                  });
    
    self = [super initWithDOMNode:view];
    if (self)
    {
    }
    return self;
}

- (UITableViewCell *)tableView:(UITableView *)tableView cellForRowAtIndexPath:(NSIndexPath *)indexPath
{
    return (UITableViewCell*)[[children objectAtIndex:[indexPath indexAtPosition:1]] cell];
}

- (NSInteger)tableView:(UITableView *)tableView numberOfRowsInSection:(NSInteger)section
{
    if (section == 0)
        return [children count];
    return 0;
}

-(void)appendChild:(ReactComponent *)child
{
    [child setParent:self];
    [children addObject:child];
}


@end
