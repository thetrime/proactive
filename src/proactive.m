//
//  proactive.c
//  proactive
//
//  Created by Matt Lilley on 5/01/17.
//  Copyright © 2017 Matt Lilley. All rights reserved.
//

#include <stdio.h>
#include <Proscript/proscript2.h>
#include "PrologEngine.h"
#include "PrologState.h"
#include "Prolog.h"
#include "ReactWidget.h"
#import <UIKit/UIKit.h>
#import <YogaKit/YogaKit.h>

void render_with_engine(PrologEngine* engine, NSString* url, PrologState* initialProps, NSString* rootElementId, UIView* container)
{
    [engine configureWithUrl:url rootElementId:rootElementId thenCall:^(int status)
        {
            if (status == 1)
            {
             ReactWidget* widget = [[ReactWidget alloc] initWithParent:NULL engine:engine elementId:rootElementId props:initialProps];
             dispatch_async(dispatch_get_main_queue(),
                            ^{
                                container.yoga.isEnabled = YES;
                                [container setBackgroundColor:[UIColor blackColor]];
                                [container addSubview:[widget getDOMNode]];                                
                                [container.yoga applyLayout];
                                [container setNeedsDisplay];
                            });
            }
        }
     ];
}


int render(NSString* url, NSString* propSpec, NSString* rootElementId, UIView* container)
{
    dispatch_queue_t queue = dispatch_queue_create("myqueue", NULL);
    PrologState* initialProps = [PrologState emptyState];
    if (propSpec != NULL)
    {
        NSString* p = [propSpec stringByRemovingPercentEncoding];
        initialProps = [[PrologState alloc] initWithTerm:[Prolog localTermFromString:p]];
    }
    PrologEngine* engine = [[PrologEngine alloc] init];
    dispatch_async(queue, ^{
        render_with_engine(engine, url, initialProps, rootElementId, container);});
    return 1;
}
