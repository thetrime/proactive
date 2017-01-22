//
//  Row.h
//  proactive
//
//  Created by Matt Lilley on 23/01/17.
//  Copyright © 2017 Matt Lilley. All rights reserved.
//

#import "ReactComponent.h"

@interface Row : ReactComponent
{
    UITableViewCell* view;
}
-(UITableViewCell*)cell;
@end
