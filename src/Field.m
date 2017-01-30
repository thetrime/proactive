//
//  Field.m
//  proactive
//
//  Created by Matt Lilley on 21/01/17.
//  Copyright © 2017 Matt Lilley. All rights reserved.
//


#import "Field.h"
#import "PrologObject.h"
#import "Prolog.h"
#import "Constants.h"
#import "ReactWidget.h"
#import <YogaKit/YogaKit.h>


@implementation Field


-(id) init
{
    dispatch_sync(dispatch_get_main_queue(),
                  ^{
                      view = [[UITextField alloc] init];
                      [view setDelegate:self];
                      [view setBorderStyle:UITextBorderStyleLine];
                      view.yoga.isEnabled = YES;
                      changeListener = -1;
                      cursorLocation = nil;
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
    if (properties[@"value"] != nil)
    {
        PrologObject* o = properties[@"value"];
        dispatch_queue_t d = dispatch_get_main_queue();
        dispatch_async(d,
                      ^{
                          view.text = [Prolog atomString:[o value]];
                          if(cursorLocation)
                          {
                              // set start/end location to same spot so that nothing is highlighted
                              [view setSelectedTextRange:[view textRangeFromPosition:cursorLocation toPosition:cursorLocation]];
                          }
                          cursorLocation = nil;
                      });
    }
    if (properties[@"onChange"] != nil)
    {
        PrologObject* o = properties[@"onChange"];
        if (changeListener != -1)
            [Prolog freeLocal:changeListener];
        if ([Constants isNull:[o value]])
             changeListener = -1;
        else
             changeListener = [Prolog makeLocal:[o value]];

    }
}

-(BOOL) textFieldShouldReturn:(UITextField *)textField{
    
    [textField resignFirstResponder];
    return YES;
}

- (BOOL)textField:(UITextField *)textField shouldChangeCharactersInRange:(NSRange)range replacementString:(NSString *)string
{
    NSString* proposedString = [textField.text stringByReplacingCharactersInRange:range withString:string];
    if (changeListener != 0)
    {
        NSLog(@"Would have been %@ %@", proposedString, owner);
        beginning = view.beginningOfDocument;
        cursorLocation = [view positionFromPosition:beginning offset:(range.location + string.length)];
        [owner triggerEvent:changeListener withData:[ReactComponent serialize:@{@"value": [PrologObject intern:[Prolog makeAtomFrom:proposedString]]}] thenCall:^(int rc) {}];
    }
    return NO;
}

@end
